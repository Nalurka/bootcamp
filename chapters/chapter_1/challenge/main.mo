import Buffer "mo:base/Buffer";

actor {

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
};