-module(bank).
-export([start/2]).

start(Name, Funds) ->
    register(Name, self()),
    loop(Name, Funds).

loop(Name, Funds) ->
    receive
        {loan_request, From, Customer, Amount} ->
            case Amount =< Funds of
                true ->
                    From ! {loan_result, grant, Name, Amount},
                    master ! {response, grant, Name, Customer, Amount},
                    loop(Name, Funds - Amount);
                false ->
                    From ! {loan_result, deny, Name, Amount},
                    master ! {response, deny, Name, Customer, Amount},
                    loop(Name, Funds)
            end
    after 10000 ->
        master ! {bank_done, Name, Funds}
    end.