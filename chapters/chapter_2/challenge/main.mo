import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Iter "mo:base/Iter";
import Types "types";
actor {

    type Member = Types.Member;
    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;
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

};