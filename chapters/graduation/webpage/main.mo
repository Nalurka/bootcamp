import Types "types";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Blob "mo:base/Blob";

actor Webpage {
    type Result<A, B> = Result.Result<A, B>;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

    stable var manifesto : Text = "Let's graduate!";
    stable var daoPrincipal : Principal = Principal.fromText("ofoea-eyaaa-aaaab-qab6a-cai"); // À remplacer après déploiement

    public query func http_request(request : HttpRequest) : async HttpResponse {
        {
            status_code = 200;
            headers = [("Content-Type", "text/plain")];
            body = Text.encodeUtf8(manifesto);
            streaming_strategy = null;
        }
    };

    public shared({ caller }) func setManifesto(newManifesto : Text) : async Result<(), Text> {
        if (caller != daoPrincipal) {
            return #err("Unauthorized");
        };
        manifesto := newManifesto;
        #ok()
    };
};