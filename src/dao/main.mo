import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Buffer "mo:base/Buffer";
import Array "mo:base/Array";
import Nat "mo:base/Nat";
import Types "types";
import Vector "mo:vector";

actor DAO {

    type Result<A, B> = Result.Result<A, B>;
    type Member = Types.Member;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Proposal = Types.Proposal;
    type Vote = Types.Vote;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;
    type Stats = Types.Stats;

    let Token = actor ("jaamb-mqaaa-aaaaj-qa3ka-cai") : actor {
        balanceOf : shared query Principal -> async Nat;
        balanceOfArray : shared query [Principal] -> async [Nat];
        burn : shared (Principal, Nat) -> async Result<(), Text>;
        mint : shared (Principal, Nat) -> async Result<(), Text>;
        tokenName : shared query () -> async Text;
        tokenSymbol : shared query () -> async Text;
        totalSupply : shared query () -> async Nat;
        transfer : shared (Principal, Principal, Nat) -> async Result<(), Text>;
    };

    stable var manifesto = "DAOs are the biggest innovation in the field of governance since the invention of the LLC or perhaps even democracy itself. Just like the steam engine made the Industrial Revolution possible by harnessing physical power, DAOs harness political power and make a Web3 revolution possible. This could fundamentally change how we organize resources, people and capital with the end goal of creating a more stable, flourishing, collaborative and fair civilisation.";
    stable let name = "BloomBytes Agency DAO";

    stable var sMembers : [(Principal, Member)] = [];
    let members = HashMap.fromIter<Principal, Member>(sMembers.vals(), 0, Principal.equal, Principal.hash);

    stable let proposals = Vector.new<Proposal>();
    stable var nextProposalId : ProposalId = 0;

    system func preupgrade() {
        sMembers := Iter.toArray(members.entries());
    };

    system func postupgrade() {
        sMembers := [];
    };

    // Returns the name of the DAO
    public query func getName() : async Text {
        return name;
    };

    // Returns the name of the DAO
    public query func getNextProposalId() : async Nat {
        return nextProposalId;
    };

    // Returns the manifesto of the DAO
    public query func getManifesto() : async Text {
        return manifesto;
    };

    // Register a new member in the DAO with the given name and principal of the caller
    // Airdrop 10 MBC tokens to the new member
    // New members are always Student
    // Returns an error if the member already exists
    public shared ({ caller }) func registerMember(name : Text) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                let member = {
                    name = name;
                    role = #Student;
                };
                members.put(caller, member);
                await Token.mint(caller, 10);
            };
            case (_) {
                return #err("Member already exist");
            };
        };
    };

    // Get the member with the given principal
    // Returns an error if the member does not exist
    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (members.get(p)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                #ok(member);
            };
        };
    };

    // Graduate the student with the given principal
    // Returns an error if the student does not exist or is not a student
    // Returns an error if the caller is not a mentor
    public shared ({ caller }) func graduate(student : Principal) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                if (member.role != #Mentor) {
                    return #err("Only mentors can graduate a student");
                };
            };
        };
        switch (members.get(student)) {
            case (null) {
                return #err("Student does not exist");
            };
            case (?member) {
                switch (member.role) {
                    case (#Student) {
                        members.put(
                            student,
                            {
                                name = member.name;
                                role = #Graduate;
                            },
                        );
                        return #ok();
                    };
                    case (_) {
                        return #err("Member is not a student");
                    };
                };
            };
        };
    };

    // Create a new proposal and returns its id
    // Returns an error if the caller is not a mentor or doesn't own at least 1 MBC token
    public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                if (member.role != #Mentor) {
                    return #err("Only mentors can create a proposal");
                };
            };
        };

        if ((await Token.balanceOf(caller)) < 1) {
            return #err("Insufficient funds");
        };

        switch (content) {
            case (#AddMentor(p)) {
                switch (members.get(p)) {
                    case (null) {
                        return #err("Member to become mentor does not exist");
                    };
                    case (?member) {
                        switch (member.role) {
                            case (#Graduate) {};
                            case (_) {
                                return #err("Only graduates can become a mentor");
                            };
                        };
                    };
                };
            };
            case (_) {};
        };

        let proposal : Proposal = {
            content = content;
            creator = caller;
            created = Time.now();
            executed = null;
            votes = [];
            status = #Open;
        };

        nextProposalId += 1;
        Vector.add(proposals, proposal);

        let _ = await Token.burn(caller, 1);

        #ok(nextProposalId - 1);
    };

    // Get the proposal with the given id
    // Returns an error if the proposal does not exist
    public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
        if (id >= nextProposalId) {
            return #err("Proposal does not exist");
        };
        #ok(Vector.get(proposals, id));
    };

    // Returns all the proposals
    public query func getAllProposal() : async [Proposal] {
        Vector.toArray(proposals);
    };

    // Vote for the given proposal
    // Returns an error if the proposal does not exist or the member is not allowed to vote
    public shared ({ caller }) func voteProposal(proposalId : ProposalId, vote : Vote) : async Result<(), Text> {
        switch (members.get(vote.member)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                switch (member.role) {
                    case (#Student) {
                        return #err("Student's are not allowed to vote");
                    };
                    case (_) {};
                };
            };
        };

        if (proposalId >= nextProposalId) {
            return #err("Proposal does not exist");
        };

        var proposal = Vector.get(proposals, proposalId);

        if (proposal.status != #Open) {
            return #err("Proposal is not open");
        };

        let voters = Buffer.fromArray<Vote>(proposal.votes);

        for (voter in voters.vals()) {
            if (voter.member == vote.member) {
                return #err("Member has already voted");
            };
        };

        voters.add(vote);
        var status = proposal.status;

        let principals = Iter.map(voters.vals(), func(x : Vote) : Principal { x.member });
        let balances = await Token.balanceOfArray(Iter.toArray(principals));

        var supporters = 0;
        var opponents = 0;

        var idx = 0;
        for (voter in voters.vals()) {
            if (voter.vote) {
                supporters += balances[idx];
            } else {
                opponents += balances[idx];
            };
            idx += 1;
        };

        var executed = proposal.executed;

        if (supporters >= 100) {
            status := #Accepted;
            switch (proposal.content) {
                case (#ChangeManifesto(t)) {
                    manifesto := t;
                };
                case (#AddMentor(p)) {
                    switch (members.get(p)) {
                        case (null) {
                            return #err("Member to become mentor does not exist");
                        };
                        case (?member) {
                            switch (member.role) {
                                case (#Graduate) {
                                    members.put(
                                        p,
                                        {
                                            name = member.name;
                                            role = #Mentor;
                                        },
                                    );
                                };
                                case (_) {
                                    return #err("Only graduates can become a mentor");
                                };
                            };
                        };
                    };
                };
            };
            executed := ?Time.now();
        } else if (opponents >= 100) {
            status := #Rejected;
        };

        proposal := {
            content = proposal.content;
            creator = proposal.creator;
            created = proposal.created;
            executed = executed;
            votes = Buffer.toArray(voters);
            status = status;
        };

        #ok(Vector.put(proposals, proposalId, proposal));
    };

    // Returns the webpage of the DAO when called from the browser
    public query func http_request(request : HttpRequest) : async HttpResponse {
        return ({
            status_code = 200;
            headers = [("Content-Type", "text/html; charset=UTF-8")];
            body = Text.encodeUtf8(manifesto);
            streaming_strategy = null;
        });
    };

    public query func getStats() : async Stats {
        {
            name = name;
            numberOfMembers = members.size();
            manifesto = manifesto;
            socialLinkDAO = "https://twitter.com/netrobeweb";
            socialLinkBuilder = "https://twitter.com/netrobeweb";
            picture = "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";
            logo = "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";
        };
    };

    // Initialize entities
    public shared ({ caller }) func init(name : Text) : async (Result<(), Text>) {
        if (Principal.isAnonymous(caller)) {
            return #err("You must not be anonymous");
        };

        let mentor = Principal.fromText("gth2f-eyaaa-aaaaj-qa2pq-cai");
        members.put(
            mentor,
            {
                name = "motoko_bootcamp";
                role = #Mentor;
            },
        );
        let _ = await Token.mint(mentor, 10);

        let student1 = Principal.fromText("4ey3h-nplzm-vyocb-xfjab-wwsos-ejl4y-27juy-qresy-emn7o-eczpl-pae");
        members.put(
            student1,
            {
                name = "Motoko";
                role = #Student;
            },
        );
        let _ = await Token.mint(student1, 10);

        let student2 = Principal.fromText("im5mh-xqo25-qfehz-urydn-n7fpm-ylmaw-n6fld-h2xdx-46pcp-tcuti-eqe");
        members.put(
            student2,
            {
                name = "Seb";
                role = #Student;
            },
        );
        let _ = await Token.mint(student2, 10);

        members.put(
            caller,
            {
                name = name;
                role = #Student;
            },
        );
        let _ = await Token.mint(caller, 10);
    };

    public shared ({ caller }) func becomeMentor(name : Text) : async () {
        members.put(
            caller,
            {
                name = name;
                role = #Mentor;
            },
        );
    };

};
