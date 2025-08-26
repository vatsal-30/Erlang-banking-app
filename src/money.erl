-module(money).
-export([start/1]).

start([CustomerFile, BankFile]) ->
    {ok, CustomerInfo} = file:consult(CustomerFile),
    {ok, BankInfo} = file:consult(BankFile),
    io:format("** The financial market is opening for the day **~n~n"),
    io:format("Starting transaction log...~n~n"),
    register(master, self()),

    lists:foreach(fun({Name, Amount}) ->
        spawn(bank, start, [Name, Amount])
    end, BankInfo),

    [spawn(customer, start, [Name, Goal, [B || {B, _} <- BankInfo], self()])
     || {Name, Goal} <- CustomerInfo],

    collect(length(CustomerInfo), length(BankInfo), #{}, BankInfo, #{}).

collect(0, 0, CustomerState, BankInfo, FinalBalances) ->
    io:format("~n~n** Banking Report **~n~n"),
    TotalGoal = lists:sum([G || {_, {G, _}} <- maps:to_list(CustomerState)]),
    TotalGot = lists:sum([R || {_, {_, R}} <- maps:to_list(CustomerState)]),

    io:format("Customers:~n"),
    lists:foreach(fun({Name, {G, R}}) ->
        io:format(" ~p: objective ~p, received ~p~n", [Name, G, R])
    end, maps:to_list(CustomerState)),
    io:format(" -----~n Total: objective ~p, received ~p~n~n", [TotalGoal, TotalGot]),

    io:format("Banks:~n"),
    lists:foreach(fun({Name, Orig}) ->
        Final = maps:get(Name, FinalBalances, Orig),
        io:format(" ~p: original ~p, balance ~p~n", [Name, Orig, Final])
    end, BankInfo),
    io:format(" -----~n Total: original ~p, loaned ~p~n~n",
              [lists:sum([O || {_, O} <- BankInfo]), TotalGot]),
    io:format("The financial market is closing for the day...~n");

collect(0, BankLeft, CMap, BInfo, BMap) ->
    receive
        {bank_done, Name, FinalBalance} ->
            collect(0, BankLeft - 1, CMap, BInfo, maps:put(Name, FinalBalance, BMap));
        {request, _, _, _} -> collect(0, BankLeft, CMap, BInfo, BMap);
        {response, _, _, _, _} -> collect(0, BankLeft, CMap, BInfo, BMap)
    end;

collect(CustomersLeft, BanksLeft, CMap, BInfo, BMap) ->
    receive
        {request, Customer, Amount, Bank} ->
            io:format("? ~p requests a loan of ~p dollar(s) from the ~p bank~n", [Customer, Amount, Bank]),
            collect(CustomersLeft, BanksLeft, CMap, BInfo, BMap);

        {response, grant, Bank, Customer, Amount} ->
            io:format("$ The ~p bank approves a loan of ~p dollar(s) to ~p~n", [Bank, Amount, Customer]),
            NewState = maps:update_with(Customer, fun({G, R}) -> {G, R + Amount} end, {0, Amount}, CMap),
            collect(CustomersLeft, BanksLeft, NewState, BInfo, BMap);

        {response, deny, Bank, Customer, Amount} ->
            io:format("$ The ~p bank denies a loan of ~p dollar(s) to ~p~n", [Bank, Amount, Customer]),
            collect(CustomersLeft, BanksLeft, CMap, BInfo, BMap);

        {done, Customer, Goal, Received} ->
            collect(CustomersLeft - 1, BanksLeft, maps:put(Customer, {Goal, Received}, CMap), BInfo, BMap)
    end.