-module(customer).
-export([start/4]).

start(Name, Goal, Banks, Master) ->
    timer:sleep(200),
    rand:seed(exsss),
    loop(Name, Goal, 0, Banks, Master).

loop(Name, 0, Received, _, Master) ->
    Master ! {done, Name, Received, Received};
loop(Name, Goal, Received, [], Master) ->
    Master ! {done, Name, Goal + Received, Received};
loop(Name, Goal, Received, Banks, Master) ->
    timer:sleep(rand:uniform(91) + 9),
    Bank = lists:nth(rand:uniform(length(Banks)), Banks),
    Amount = rand:uniform(min(50, Goal)),
    whereis(Bank) ! {loan_request, self(), Name, Amount},
    Master ! {request, Name, Amount, Bank},
    receive
        {loan_result, grant, Bank, Amount} ->
            loop(Name, Goal - Amount, Received + Amount, Banks, Master);
        {loan_result, deny, Bank, Amount} ->
            loop(Name, Goal, Received, lists:delete(Bank, Banks), Master)
    end.