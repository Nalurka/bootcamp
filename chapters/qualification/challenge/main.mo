actor Marine {
    let name : Text = "Marine";
    var message : Text = "I just want to learn new technology";
    public func setMessage(newMessage : Text) : async (){
        message := newMessage;
        return;
    };
    public query func getMessage() : async Text {
        return message;
    };
    public query func getName() : async Text {
        return name;
    };
} 
    