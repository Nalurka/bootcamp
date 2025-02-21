import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
import TrieMap "mo:base/TrieMap";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Buffer "mo:base/Buffer";
import Nat64 "mo:base/Nat64";
import Iter "mo:base/Iter";
import Blob "mo:base/Blob";
import Debug "mo:base/Debug";
import Option "mo:base/Option";
import Time "mo:base/Time";
import Array "mo:base/Array";
import Types "types";
actor {
    // For this level we need to make use of the code implemented in the previous projects.
    // The voting system will make use of previous data structures and functions.

    /////////////////
    //   TYPES    //
    ///////////////
    type Member = Types.Member;
    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;
    type Proposal = Types.Proposal;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Vote = Types.Vote;
    type DAOStats = Types.DAOStats;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

    /////////////////
    // PROJECT #1 //
    ///////////////
    let name : Text = "DAO project";
    var manifesto : Text = "Understand the DAO and motoko for web3";
    var goals : Buffer.Buffer<Text> = Buffer.Buffer<Text>(10);

    public shared query func getName() : async Text {
        return name;
    };

    public shared query func getManifesto() : async Text {
        return manifesto;
    };

    public func setManifesto(newManifesto : Text) : async () {
        manifesto := newManifesto;
        return;
    };

    public func addGoal(newGoal : Text) : async () {
        goals.add(newGoal);
        return;
    };

    public shared query func getGoals() : async [Text] {
        return Buffer.toArray(goals);
    };

    /////////////////
    // PROJECT #2 //
    ///////////////
    let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

    public shared ({ caller }) func addMember(member : Member) : async Result<(), Text> {
        switch(members.get(caller)){
            case(null){
                members.put(caller, member);
                return #ok();
            };
            case(? oldMember){
                return #err("Already linked");
            }
        };
    };

    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch(members.get(p)){
            case(null){
                return #err("No member linked");
            };
            case(? member){
                return #ok(member);
            };
        };
    };

    public shared ({ caller }) func updateMember(member : Member) : async Result<(), Text> {
        switch(members.get(caller)){
            case(null){
                return #err("No member linked");
            };
            case(? oldMember){
                members.put(caller, member);
                return #ok();
            }
        };
    };

    public query func getAllMembers() : async [Member] {
        let all = members.vals();
        return Iter.toArray(all);
    };

    public query func numberOfMembers() : async Nat {
        return members.size();
    };

    public shared ({ caller }) func removeMember() : async Result<(), Text> {
        switch(members.get(caller)){
            case(null){
                return #err("No member linked");
            };
            case(? oldMember){
                members.delete(caller);
                return #ok();
            }
        };
    };

    /////////////////
    // PROJECT #3 //
    ///////////////
    let wallet = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);
    let Tokenname : Text = "DevToken3000";
    let symbol : Text = "DT3";

    public query func tokenName() : async Text {
        return Tokenname;
    };

    public query func tokenSymbol() : async Text {
        return symbol;
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let ownerBalance = Option.get (wallet.get(owner), 0);
        wallet.put(owner, ownerBalance + amount);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let ownerBalance = Option.get(wallet.get(owner), 0);
        if(amount > ownerBalance){
            return #err("Too poor for that");
        };
        wallet.put(owner, ownerBalance - amount);
        return #ok();
    };

    func _burn(owner: Principal, amount:Nat) : () {
        let balance = Option.get(wallet.get(owner), 0);
        wallet.put(owner, balance-amount);
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        let balanceOrigins = Option.get(wallet.get(from), 0);
        let balanceDestination = Option.get(wallet.get(to), 0);
        if(balanceOrigins < amount){
            return #err("Still too poor");
        };
        wallet.put(from, balanceOrigins - amount);
        wallet.put(to, balanceDestination + amount);
        return #ok();
    };

    public query func balanceOf(account : Principal) : async Nat {
        return Option.get(wallet.get(account), 0);
    };

    public query func totalSupply() : async Nat {
        var balanceTotal = 0;
        for(balance in wallet.vals()){
            balanceTotal := balanceTotal + balance;
        };
        return balanceTotal;
    };
    /////////////////
    // PROJECT #4 //
    ///////////////
    let proposals = HashMap.HashMap<ProposalId, Proposal>(0,Nat64.equal, Nat64.toNat32);
    stable var nextProposalId : Nat64 = 0;

    public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
        if(Option.isNull(members.get(caller))){
            return #err("You need to create a member");
        };

        let balanceCaller = Option.get(wallet.get(caller), 0);
        if(balanceCaller < 1){
            return #err("You need the right balance");
        };

        let newProposal = {
            id = nextProposalId;
            content;
            creator = caller;
            created = Time.now();
            executed = null;
            votes = [];
            voteScore = 0;
            status = #Open;
        };

        proposals.put(nextProposalId, newProposal);
        _burn(caller, 1);
        nextProposalId += 1;
        return #ok(nextProposalId - 1);
    };

    public query func getProposal(proposalId : ProposalId) : async ?Proposal {
        return proposals.get(proposalId);
    };

    public query func getAllProposals() : async [Proposal] {
        return Iter.toArray(proposals.vals());
    };

    public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
        if(Option.isNull(members.get(caller))){
            return #err("You need to create a member");
        };
        switch(proposals.get(proposalId)){
            case(null){
                return #err("No proposal found");
            };
            case(? proposal){
                if (_hasVoted(proposal, caller)){
                    return #err("You can only vote once per proposal");
                };
                let newProposal = _newProposal(proposal, caller, yesOrNo);
                proposals.put(proposal.id ,newProposal);
                if(newProposal.status == #Accepted){
                    _execute(newProposal);
                };
                return #ok;
            };
        };
    };


    func _hasVoted(proposal: Proposal, p: Principal) : Bool {
        for(vote in proposal.votes.vals()){
            if(vote.member == p){
                return true;
            };
        };
        return false;
    };

    func _newProposal(proposal : Proposal, voter : Principal, yesOrNo : Bool) : Proposal{
        let votingPower = Option.get(wallet.get(voter), 0);
        let multiplier = switch(yesOrNo){
            case(true){1};
            case(false){-1};
        };

        let callerVoteScore = votingPower * multiplier;
        let newVotes = Buffer.fromArray<Vote>(proposal.votes);

        newVotes.add({
            member = voter;
            votingPower;
            yesOrNo;
        });

        let newScore = proposal.voteScore + callerVoteScore;
        
        let newStatus = if(newScore >= 100){
            #Accepted;
        } else if (newScore <= -100){
            #Rejected;
        }
        else{
            #Open;
        };

        let newProposal = {
            id = proposal.id;
            content = proposal.content;
            creator = proposal.creator;
            created = proposal.created;
            executed = proposal.executed;
            votes = Buffer.toArray(newVotes);
            voteScore = newScore;
            status = newStatus;
        };
        return newProposal;
    };


    func _execute(proposal: Proposal) : () {
        switch(proposal.content){
            case(#ChangeManifesto(newManifesto)){
                manifesto := newManifesto;
            };
            case(#AddGoal(newGoal)){
                goals.add(newGoal);
            };
        };
        let newProposal = {
            id = proposal.id;
            content = proposal.content;
            creator = proposal.creator;
            created = proposal.created;
            executed = ?Time.now();
            votes = proposal.votes;
            voteScore = proposal.voteScore;
            status = proposal.status;
            
            };
            proposals.put(proposal.id, newProposal);
            return;
    };


    /////////////////
    // PROJECT #5 //
    ///////////////
    let logo : Text = "<svg version='1.0' xmlns='http://www.w3.org/2000/svg'
 width='512.000000pt' height='512.000000pt' viewBox='0 0 512.000000 512.000000'
 preserveAspectRatio='xMidYMid meet'>
<g transform='translate(0.000000,512.000000) scale(0.100000,-0.100000)'
fill='#000000' stroke='none'>
<path d='M2895 5049 c-555 -79 -1151 -378 -1669 -838 -38 -33 -72 -59 -76 -56
-13 8 -110 172 -110 186 0 8 28 78 62 157 68 157 69 175 16 215 l-28 21 -157
-37 c-87 -20 -161 -37 -166 -37 -4 0 -44 33 -90 73 -156 137 -156 137 -191
137 -19 0 -44 -8 -57 -18 -20 -17 -23 -32 -35 -180 -7 -89 -15 -166 -18 -171
-3 -5 -65 -44 -136 -86 -141 -83 -166 -107 -155 -154 9 -42 26 -53 170 -112
72 -29 137 -59 146 -68 10 -9 29 -75 48 -161 34 -160 52 -190 113 -190 34 0
52 17 171 158 l77 92 49 0 c27 0 62 -3 79 -6 l31 -6 -90 -101 c-711 -792
-1032 -1734 -808 -2375 17 -48 46 -115 65 -149 l33 -63 -45 7 c-67 9 -114 -26
-114 -86 0 -9 36 -78 80 -152 44 -74 80 -139 80 -145 0 -7 -29 -77 -65 -156
-60 -133 -64 -148 -54 -177 6 -18 24 -38 40 -46 26 -14 41 -12 189 21 l161 37
42 -40 c171 -161 199 -183 233 -183 56 0 81 33 89 116 3 38 9 97 13 132 l7 63
200 -182 c110 -99 223 -198 250 -219 122 -93 299 -165 484 -196 152 -25 418
-16 599 20 441 89 888 306 1347 656 10 8 137 -38 155 -56 5 -5 28 -93 51 -194
36 -166 43 -188 70 -212 34 -32 72 -36 111 -10 15 9 74 74 132 144 58 69 114
133 125 140 17 12 46 12 203 -3 101 -10 193 -15 206 -12 31 7 72 59 72 92 0
17 -37 86 -100 186 -55 88 -100 170 -100 183 0 13 32 96 70 183 80 183 85 215
41 260 -16 16 -39 29 -51 29 -11 0 -84 -16 -162 -35 -77 -20 -142 -35 -143
-33 -2 2 28 44 66 93 341 442 561 917 645 1390 25 140 25 436 1 559 -38 185
-121 366 -227 491 -45 53 -947 847 -1043 918 -117 87 -314 163 -481 186 -90
13 -363 13 -451 0z m315 -33 c0 -2 -69 -123 -153 -268 l-153 -263 -110 -2
c-128 -2 -240 -16 -343 -44 -40 -11 -75 -18 -77 -17 -4 5 101 476 108 483 21
21 313 89 448 104 79 9 280 14 280 7z m265 -41 c168 -42 300 -116 454 -255 42
-37 238 -208 436 -380 198 -172 386 -338 418 -369 120 -116 212 -286 257 -475
23 -98 40 -300 31 -373 l-6 -52 -407 361 -408 361 0 58 c0 52 -2 59 -19 59
-16 0 -21 -10 -26 -53 l-7 -54 -306 -92 c-168 -51 -311 -95 -319 -98 -10 -4
-13 18 -13 111 0 170 -38 327 -104 427 l-25 36 200 144 c133 95 199 149 199
161 0 10 -7 18 -14 18 -12 0 -393 -267 -430 -301 -6 -5 9 -44 37 -98 70 -132
89 -215 92 -396 2 -126 5 -150 18 -153 8 -2 157 40 332 92 174 53 318 96 321
96 10 0 3 -59 -21 -174 -68 -335 -216 -687 -427 -1012 l-53 -82 -250 104
c-137 57 -258 103 -267 104 -10 0 -40 -31 -71 -73 -147 -198 -387 -446 -564
-582 -62 -47 -92 -77 -92 -91 -1 -11 24 -143 54 -294 30 -151 55 -279 55 -285
0 -15 -232 -153 -387 -230 -160 -80 -325 -145 -484 -190 -129 -38 -305 -72
-314 -63 -4 3 39 153 95 332 55 180 98 331 95 337 -4 6 -53 7 -129 3 -79 -5
-152 -2 -206 5 -95 14 -225 59 -282 96 -21 14 -43 25 -50 25 -7 -1 -46 -46
-86 -101 -69 -95 -74 -100 -91 -85 -18 16 -57 21 -85 10 -9 -3 -55 -52 -103
-107 -130 -150 -115 -140 -186 -132 -109 11 -111 12 -143 68 -62 108 -133 318
-134 398 0 10 110 98 284 225 l283 209 6 135 c6 157 25 263 74 414 22 68 31
112 25 118 -6 6 -103 -16 -262 -57 -138 -37 -254 -64 -257 -61 -14 14 144 345
243 507 107 178 270 407 290 408 6 1 91 -35 190 -78 98 -43 184 -76 190 -72 6
3 40 40 75 81 96 114 261 273 387 373 61 49 112 95 112 102 0 7 -30 94 -66
194 -36 100 -63 188 -60 196 6 17 199 144 341 226 173 99 487 240 501 226 4
-3 -17 -114 -46 -246 -29 -132 -50 -247 -46 -257 4 -11 13 -14 29 -10 185 55
360 81 488 74 64 -4 81 -2 92 11 8 9 85 139 172 290 l158 273 60 -7 c33 -3
101 -17 152 -30z m-2865 -242 c61 -54 123 -103 138 -109 23 -10 52 -6 179 25
157 39 183 39 183 -1 0 -11 -27 -83 -61 -159 -42 -99 -58 -146 -53 -162 3 -12
42 -79 85 -150 44 -70 79 -134 79 -142 0 -31 -34 -34 -188 -19 -125 13 -155
13 -176 2 -13 -7 -66 -62 -116 -123 -100 -119 -122 -137 -145 -114 -8 8 -28
79 -45 159 -18 80 -37 154 -44 165 -8 11 -74 44 -162 80 -136 55 -149 62 -152
87 -3 24 8 32 126 102 71 42 137 85 147 96 15 17 21 51 31 176 12 151 21 184
50 184 7 0 63 -44 124 -97z m979 -515 l63 -173 -38 -30 c-157 -124 -337 -295
-457 -434 l-46 -55 -170 73 -170 74 20 26 c11 14 68 79 125 144 l105 117 69 0
c62 0 70 2 89 26 23 29 27 74 10 103 -12 19 -8 24 116 133 66 58 206 168 214
168 4 0 36 -78 70 -172z m3054 -834 c216 -191 398 -353 406 -360 13 -11 13
-27 3 -104 -29 -216 -116 -485 -238 -740 -67 -139 -205 -383 -223 -394 -5 -3
-828 638 -860 669 -2 3 23 46 56 97 221 342 378 728 439 1076 9 56 19 102 21
102 2 0 181 -156 396 -346z m-3989 -681 c-39 -126 -65 -280 -68 -401 l-3 -113
-264 -194 -264 -194 -8 57 c-11 72 7 298 33 432 25 120 87 341 101 355 8 8
449 130 485 134 8 1 3 -27 -12 -76z m2779 -166 c119 -49 217 -94 217 -100 0
-16 -156 -219 -262 -342 -124 -143 -364 -377 -504 -491 -122 -100 -283 -218
-289 -213 -1 2 -28 126 -57 276 l-55 271 51 39 c186 140 399 354 559 561 68
88 82 102 101 96 11 -4 119 -47 239 -97z m700 -457 c234 -184 426 -339 426
-343 2 -25 -243 -337 -265 -337 -7 0 -77 56 -153 124 -86 76 -151 127 -170
131 -35 7 -85 -15 -102 -46 -6 -11 -17 -107 -25 -212 -8 -105 -18 -195 -23
-200 -5 -5 -83 -53 -172 -106 -144 -85 -165 -100 -177 -132 -12 -33 -12 -41 3
-72 14 -30 29 -41 93 -67 43 -18 81 -36 85 -40 9 -8 -159 -134 -279 -210 l-60
-38 -345 399 c-189 219 -345 402 -347 407 -1 5 46 44 105 88 209 153 419 341
596 532 116 125 279 323 337 409 19 29 38 51 41 50 3 -1 197 -153 432 -337z
m-3159 -495 c116 -55 227 -78 389 -78 99 1 137 -3 134 -11 -2 -6 -46 -149 -97
-318 -79 -258 -97 -308 -114 -312 -12 -3 -68 -6 -126 -5 -98 0 -103 1 -78 15
15 8 32 23 38 34 14 26 13 73 -2 93 -7 10 -76 45 -153 78 -77 33 -145 66 -152
72 -6 7 -23 71 -38 143 l-27 131 72 97 c39 53 73 96 76 96 3 0 38 -16 78 -35z
m3149 -105 c119 -105 154 -130 178 -130 16 0 101 18 189 40 190 48 216 49 232
15 10 -21 1 -47 -66 -203 -42 -98 -76 -187 -76 -199 0 -11 45 -92 100 -181
150 -241 149 -243 -125 -218 -162 14 -198 15 -219 4 -13 -7 -78 -76 -143 -153
-99 -117 -124 -140 -149 -143 -17 -2 -35 3 -41 10 -6 7 -29 96 -51 198 -33
145 -46 189 -64 206 -13 12 -93 49 -178 84 -184 74 -200 83 -200 115 0 30 25
49 196 149 72 42 139 85 149 96 15 17 21 52 31 183 7 89 13 176 14 194 0 37
22 63 53 63 14 0 76 -48 170 -130z m-3440 -22 c11 -9 26 -60 47 -161 16 -81
34 -156 39 -166 6 -10 72 -45 155 -81 80 -34 151 -69 158 -77 20 -25 -1 -44
-142 -124 -79 -46 -138 -86 -146 -100 -6 -13 -18 -91 -25 -172 -12 -146 -21
-177 -53 -177 -6 0 -50 35 -96 78 -140 127 -159 142 -188 142 -15 0 -91 -14
-169 -32 -118 -26 -144 -30 -157 -19 -9 8 -16 19 -16 27 0 7 27 73 61 146 33
73 61 145 61 160 1 17 -26 72 -69 145 -101 169 -97 159 -75 181 18 18 23 18
172 1 91 -11 161 -14 174 -9 12 4 51 41 86 82 80 92 152 168 160 168 3 0 14
-6 23 -12z m2244 -549 c188 -217 342 -396 342 -399 1 -8 -236 -134 -339 -180
-205 -92 -409 -158 -604 -195 -86 -16 -324 -39 -349 -33 -9 1 -149 165 -311
363 l-295 360 42 8 c359 66 715 206 1047 411 58 36 110 64 115 62 6 -1 164
-180 352 -397z m-1318 -449 c160 -195 289 -355 288 -356 -1 -2 -38 2 -82 7
-167 20 -339 80 -466 161 -57 37 -511 439 -512 454 0 5 30 27 68 49 l67 40
174 0 173 0 290 -355z'/>
<path d='M2455 4174 c-188 -40 -421 -143 -610 -269 -478 -320 -864 -846 -950
-1297 -20 -106 -20 -284 0 -363 50 -195 184 -329 381 -379 80 -21 257 -21 363
-1 607 116 1306 755 1516 1387 48 146 65 241 65 365 0 194 -42 305 -155 418
-115 115 -225 155 -425 154 -63 0 -146 -7 -185 -15z m360 -50 c173 -42 293
-161 340 -339 19 -71 21 -233 5 -325 -83 -473 -500 -1025 -1010 -1336 -166
-101 -348 -176 -510 -209 -101 -21 -264 -24 -340 -6 -144 34 -262 123 -318
241 -44 92 -55 149 -55 275 1 176 42 333 143 540 207 426 611 830 1035 1035
274 132 506 173 710 124z'/>
</g>
</svg>";

    func _getWebpage() : Text {
        var webpage = "<style>" #
        "body { text-align: center; font-family: Arial, sans-serif; background-color: #f0f8ff; color: #333; }" #
        "h1 { font-size: 3em; margin-bottom: 10px; }" #
        "hr { margin-top: 20px; margin-bottom: 20px; }" #
        "em { font-style: italic; display: block; margin-bottom: 20px; }" #
        "ul { list-style-type: none; padding: 0; }" #
        "li { margin: 10px 0; }" #
        "li:before { content: '👉 '; }" #
        "svg { max-width: 150px; height: auto; display: block; margin: 20px auto; }" #
        "h2 { text-decoration: underline; }" #
        "</style>";

        webpage := webpage # "<div><h1>" # name # "</h1></div>";
        webpage := webpage # "<em>" # manifesto # "</em>";
        webpage := webpage # "<div>" # logo # "</div>";
        webpage := webpage # "<hr>";
        webpage := webpage # "<h2>Our goals:</h2>";
        webpage := webpage # "<ul>";
        for (goal in goals.vals()) {
            webpage := webpage # "<li>" # goal # "</li>";
        };
        webpage := webpage # "</ul>";
        return webpage;
    };

    func _getMemberNames() : [Text] {
        let memberArray = Iter.toArray(members.vals());
        return Array.map<Member, Text>(memberArray, func(member :Member){member.name});
    };

    public query func getStats() : async DAOStats {
        return ({
            name = "DAO project";
            manifesto = "Understand the DAO and motoko for web3";
            goals = Buffer.toArray(goals);
            members = _getMemberNames();
            logo;
            numberOfMembers = members.size();
        });
    };

    public query func http_request(request : HttpRequest) : async HttpResponse {
        return ({
            status_code = 200;
            headers = [("Content-Type", "text/html; charset=UTF-8")];
            body = Text.encodeUtf8(_getWebpage());
            streaming_strategy = null;
        });
    };

};