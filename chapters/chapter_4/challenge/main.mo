import Result "mo:base/Result";
import Buffer "mo:base/Buffer";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Types "types";
import Nat64 "mo:base/Nat64";
import Time "mo:base/Time";
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

    /////////////////
    // PROJECT #1 //
    ///////////////
    let goals = Buffer.Buffer<Text>(0);
    let name = "Motoko Bootcamp";
    var manifesto = "Empower the next generation of builders and make the DAO-revolution a reality";

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
        Buffer.toArray(goals);
    };

    /////////////////
    // PROJECT #2 //
    ///////////////
    let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

    public shared ({ caller }) func addMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                members.put(caller, member);
                return #ok();
            };
            case (?member) {
                return #err("Member already exists");
            };
        };
    };

    public shared ({ caller }) func updateMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                members.put(caller, member);
                return #ok();
            };
        };
    };

    public shared ({ caller }) func removeMember() : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                members.delete(caller);
                return #ok();
            };
        };
    };

    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (members.get(p)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                return #ok(member);
            };
        };
    };

    public query func getAllMembers() : async [Member] {
        return Iter.toArray(members.vals());
    };

    public query func numberOfMembers() : async Nat {
        return members.size();
    };

    /////////////////
    // PROJECT #3 //
    ///////////////
    let ledger = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);

    public query func tokenName() : async Text {
        return "Motoko Bootcamp Token";
    };

    public query func tokenSymbol() : async Text {
        return "MBT";
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        ledger.put(owner, balance + amount);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        if (balance < amount) {
            return #err("Insufficient balance to burn");
        };
        ledger.put(owner, balance - amount);
        return #ok();
    };
    func _burn(owner: Principal, amount:Nat) : () {
        let balance = Option.get(ledger.get(owner), 0);
        ledger.put(owner, balance-amount);
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        let balanceFrom = Option.get(ledger.get(from), 0);
        let balanceTo = Option.get(ledger.get(to), 0);
        if (balanceFrom < amount) {
            return #err("Insufficient balance to transfer");
        };
        ledger.put(from, balanceFrom - amount);
        ledger.put(to, balanceTo + amount);
        return #ok();
    };

    public query func balanceOf(owner : Principal) : async Nat {
        return (Option.get(ledger.get(owner), 0));
    };

    public query func totalSupply() : async Nat {
        var total = 0;
        for (balance in ledger.vals()) {
            total += balance;
        };
        return total;
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

        let balanceCaller = Option.get(ledger.get(caller), 0);
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
        let votingPower = Option.get(ledger.get(voter), 0);
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

};