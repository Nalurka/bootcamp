import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Time "mo:base/Time";
import Map "mo:map/Map";
import Debug "mo:base/Debug";
import Iter "mo:base/Iter";
import Types "types";
import Int "mo:base/Int";
import Array "mo:base/Array";

actor {
    type Result<A, B> = Result.Result<A, B>;
    type Member = Types.Member;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Proposal = Types.Proposal;
    type Vote = Types.Vote;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

    stable let canisterIdWebpage : Principal = Principal.fromText("75i2c-tiaaa-aaaab-qacxa-cai"); // À remplacer après déploiement
    stable var manifesto = "Understand the DAO and motoko for web3";
    stable let name = "DAO project";
    stable var goals : [Text] = [];
    
    stable var membersStable : [(Principal, Member)] = [];
    stable var nextProposalId : ProposalId = 0;
    stable var proposalsStable : [(ProposalId, Proposal)] = [];

    let members = Map.fromIter<Principal, Member>(membersStable.vals(), Map.phash);
    let proposals = Map.fromIter<ProposalId, Proposal>(proposalsStable.vals(), Map.nhash);

    let tokenCanister = actor("jaamb-mqaaa-aaaaj-qa3ka-cai") : actor {
        mint : (Principal, Nat) -> async Result.Result<(), Text>;
        burn : (Principal, Nat) -> async Result.Result<(), Text>;
        balanceOf : (Principal) -> async Nat;
    };

    let initialMentorPrincipal = Principal.fromText("nkqop-siaaa-aaaaj-qa3qq-cai");
    if (Map.size(members) == 0) {
        let initialMentor : Member = {
            name = "motoko_bootcamp";
            role = #Mentor;
        };
        Map.set(members, Map.phash, initialMentorPrincipal, initialMentor);
    };

    system func preupgrade() {
    membersStable := Iter.toArray(Map.entries(members)); 
    proposalsStable := Iter.toArray(Map.entries(proposals)); 
    };

    system func postupgrade() {
        membersStable := [];
        proposalsStable := [];
    };

    public query func getName() : async Text {
        name
    };

    public query func getManifesto() : async Text {
        manifesto
    };

    public query func getGoals() : async [Text] {
        goals
    };

    public shared({ caller }) func registerMember(member : Member) : async Result<(), Text> {
        if (Principal.isAnonymous(caller)) {
            return #err("Not allowed if anonymous");
        };

        switch (Map.get(members, Map.phash, caller)) {
            case (?_) #err("Member already exists");
            case null {
                let newMember = { name = member.name; role = #Student };
                Map.set(members, Map.phash, caller, newMember);
                
                switch (await tokenCanister.mint(caller, 10)) {
                    case (#err(e)) {
                        Map.delete(members, Map.phash, caller);
                        #err("Minting failed: " # e)
                    };
                    case _ #ok()
                }
            }
        }
    };

    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (Map.get(members, Map.phash, p)) {
            case (?member) #ok(member);
            case null #err("Member not found")
        }
    };

    public shared({ caller }) func graduate(student : Principal) : async Result<(), Text> {
        switch (Map.get(members, Map.phash, caller), Map.get(members, Map.phash, student)) {
            case (?{ role = #Mentor }, ?{ role = #Student; name }) {
                Map.set(members, Map.phash, student, { name; role = #Graduate });
                #ok()
            };
            case _ #err("Unauthorized")
        }
    };

    public shared({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
    switch (Map.get(members, Map.phash, caller)) {
        case (?{ role = #Mentor }) {
            let balance = await tokenCanister.balanceOf(caller); 
            if (balance < 1) {
                return #err("Insufficient tokens");
            };

            switch (await tokenCanister.burn(caller, 1)) {
                case (#ok()) {
                    let proposalId = nextProposalId;
                    nextProposalId += 1;

                    let proposal : Proposal = {
                        id = proposalId;
                        content;
                        creator = caller;
                        created = Time.now();
                        executed = null;
                        votes = [];
                        voteScore = 0;
                        status = #Open;
                    };

                    Map.set(proposals, Map.nhash, proposalId, proposal);
                    #ok(proposalId)
                };
                case (#err(e)) #err("Burn failed: " # e)
            }
        };
        case _ #err("Only mentors can create proposals, please be a mentor")
        }
    };

    public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
        switch (Map.get(proposals, Map.nhash, id)) {
            case (?proposal) #ok(proposal);
            case null #err("Not found")
        }
    };

    public query func getAllProposal() : async [Proposal] {
        Iter.toArray(Map.vals(proposals))
    };

    public shared({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
    switch (Map.get(proposals, Map.nhash, proposalId), Map.get(members, Map.phash, caller)) {
        case (?proposal, ?member) {
            
            if (proposal.status != #Open) {
                return #err("Not open");
            };

            
            if (member.role == #Student) {
                return #err("Need an other role to vote / not student");
            };

            
            let balance = await tokenCanister.balanceOf(caller);
            let votingPower = switch (member.role) {
                case (#Mentor) balance * 5;
                case (#Graduate) balance;
                case _ 0;  
            };

        
            let voteScore = proposal.voteScore + (
                if (yesOrNo) {
                    votingPower;
                } else {
                    -votingPower;
                }
            );

            let status = if (voteScore >= 100) #Accepted else if (voteScore <= -100) #Rejected else #Open;

            
            let vote : Vote = {
                member = caller;
                votingPower = votingPower;
                yesOrNo = yesOrNo;
            };

            
            let updatedProposal : Proposal = {
                id = proposal.id;
                content = proposal.content;
                creator = proposal.creator;
                created = proposal.created;
                executed = proposal.executed;
                votes = Array.append(proposal.votes, [vote]);
                voteScore = voteScore;
                status = status;
            };

            
            Map.set(proposals, Map.nhash, proposalId, updatedProposal);

        
            if (status == #Accepted) {
                await executeProposal(updatedProposal);
            };

            #ok()
        };
        case _ {
            #err("Invalid")
            }
        }
    };

    func executeProposal(proposal : Proposal) : async () {
        switch (proposal.content) {
            case (#ChangeManifesto(text)) {
                manifesto := text;
                let webpage = actor(Principal.toText(canisterIdWebpage)) : actor {
                    setManifesto : shared (Text) -> async Result<(), Text>;
                };
                ignore await webpage.setManifesto(text);
            };
            case (#AddMentor(p)) {
                switch (Map.get(members, Map.phash, p)) {
                    case (?{ role = #Graduate; name }) {
                        Map.set(members, Map.phash, p, { name; role = #Mentor })
                    };
                    case _ ()
                }
            };
            case (#AddGoal(text)) {
                goals := Array.append(goals, [text])
            }
        }
    };

    public query func getIdWebpage() : async Principal {
        canisterIdWebpage
    };
};