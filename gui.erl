-module(gui).
-compile([export_all]).
-include_lib("wx/include/wx.hrl").

init({Receive, Send}, Main) ->
    Wx = make_window(),
    io:format("WDDDDD ~p ~p", [Receive, Send]),
    Cashiers = init_cashiers(Wx, Receive, Send, length(Receive), 0,0,980,[]),
    io:format("~p", [Cashiers]),
    io:format("-----------GUI ~p", [Main]),
    loop(Wx, Cashiers, Main, [], {Receive, Send}).

make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Post office simulation", [{size,{1000,500}}]),
    End_Button = wxButton:new(Frame, ?wxID_STOP, [{label, "End simulation"}, {pos, {300,70}}]),
    Time_Text = wxStaticText:new(Frame, 0, "Time", [{pos,{300,50}}]),
    ClientsR = wxStaticText:new(Frame, 0, "", [{pos, {0, 200}}]),
    ClientsS = wxStaticText:new(Frame, 0, "", [{pos, {100, 200}}]),
    wxFrame:createStatusBar(Frame),
    wxFrame:show(Frame),
    
    wxFrame:connect(Frame, close_window),
    wxButton:connect(End_Button, command_button_clicked),

    {Server, Frame, End_Button, Time_Text, ClientsR, ClientsS}.

loop(Wx, Cashiers, Main, ClientList, RS) ->
    {Server, Frame, End_Button, Time_Text, ClientsR, ClientsS} = Wx,
    io:format("--waiting in the loop--~n", []),
    receive 
        #wx{id = ?wxID_STOP, event=#wxCommand{type = command_button_clicked}} ->
            finish(Cashiers, End_Button, Frame, Main);
        {time_passed, CurrentTime} ->
            set_time(CurrentTime, Time_Text),
            loop(Wx, Cashiers, Main, ClientList, RS);
        {client, Client} ->
            NewList = new_client(Client, ClientList, ClientsR),
            loop(Wx, Cashiers, Main, NewList, RS);
        {clients, S, R} ->
            CRList = clients(R, []),
            CSList = clients(S, []),
            wxStaticText:setLabel(ClientsR, CRList),
            wxStaticText:setLabel(ClientsS, CSList),
            loop(Wx, Cashiers, Main, ClientList, RS);
        {cashier, CashierPid} ->
            io:format("CASHIER ~p", [CashierPid]),
            show_client_at_cashier(CashierPid, Cashiers),
            loop(Wx, Cashiers, Main, ClientList, RS);
        {cashier, done, CashierPid} ->
            delete_client(CashierPid, Cashiers),
            loop(Wx, Cashiers, Main, ClientList, RS);
        {end_of_work, CashierPid} ->
            delete_client(CashierPid, ClientList),
            loop(Wx, Cashiers, Main, ClientList, RS);
        {end_day} ->
            io:format("endday"),
            lists:foreach(fun(X) -> 
                {Pid, WxText} = X, 
                wxStaticText:setLabel(WxText, ""),
                wxStaticText:destroy(WxText)
                end, Cashiers),
            {R, S} = RS,
            CashiersV2 = init_cashiers(Wx, R, S, length(R), 0,0,980,[]),
            loop(Wx, CashiersV2, Main, ClientList, RS);
        #wx{event=#wxClose{}} ->
            io:format("--closing window ~p-- ~n",[self()]),
            wxWindow:destroy(Frame),
            ok
    end.

delete_client(Cashier, Cashiers) ->
    {CashierPid, Case} = Cashier,
    lists:foreach(fun(X)->
                    {Pid, Wx_Text} = X,
                    if Pid == CashierPid ->
                        wxStaticText:setLabel(Wx_Text, cashier_out(Case));
                    true ->
                        ok
                    end
                end, Cashiers
    ).

show_client_at_cashier(Cashier, Cashiers) ->
    {CashierPid, Case} = Cashier,
    io:format("~p", [Case]),
    lists:foreach(fun(X)->
                    {Pid, Wx_Text} = X,
                    if Pid == CashierPid ->
                        wxStaticText:setLabel(Wx_Text, cashier_out(Case) ++ "\nKlient");
                    true ->
                        ok
                    end
                end, Cashiers
    ).
cashier_out(send_package) ->
    "Send |";
cashier_out(receive_package) ->
    "Receive |".


clients([], List) ->    
    List;
clients([{_, Type, _}|T], List1) ->
    List = lists:concat([List1, "\n", Type]),
    clients(T, List).

new_client({_, Type, _}, ClientList, ClientsWx) ->
    Clients = lists:concat([ClientList, "\n", Type]),
    wxStaticText:setLabel(ClientsWx, Clients),
    io:format("~p", [Clients]),
    Clients.

finish(Cashiers, End_Button, Frame, Main) ->
    destroy_cashiers(Cashiers),
    wxWindow:destroy(End_Button),
    wxFrame:setStatusText(Frame, "Simulation ended."), 
    Main ! {end_simulation},
    end_simulation(Frame).

set_time(CurrentTime, Time_Text) ->
    Time = get_time(CurrentTime),
    wxStaticText:setLabel(Time_Text, "Time: " ++ Time),
    ok.

get_time(Minutes) ->
    Hour = trunc(Minutes/60),
    Minute = Minutes - Hour*60,
    MinuteOut = minutes(integer_to_list(Minute), Minute<10),
    integer_to_list(Hour) ++ ":" ++ MinuteOut.

minutes(Minute, true) ->
    "0" ++ Minute;
minutes(Minute, false) ->
    Minute.

end_simulation(Frame) ->
    io:format("--simulation ended--~n"),
    receive
        {statistics, Results} ->
            wxStaticText:new(Frame, 0, results(Results), [{pos,{300,100}}]),
            end_simulation(Frame);
        #wx{event=#wxClose{}} ->
            io:format("--closing window ~p-- ~n",[self()]),
            wxWindow:destroy(Frame),
            ok
    end.

results({R_Sum, S_Sum}) ->
    "Ended simulation with:\n" ++ 
    % integer_to_list(Time) ++ " time simulated\n" ++ 
    integer_to_list(R_Sum+S_Sum) ++ " clients handled\n" ++
    integer_to_list(S_Sum) ++ " packages sent\n" ++
    integer_to_list(R_Sum) ++ " packages received\n".

destroy_cashiers(List) ->
    lists:foreach(fun(H) -> 
        {Pid, Wx_Text} = H,
        wxWindow:destroy(Wx_Text) end, List).

init_cashiers(_, [], [],_, _,_,_, List) -> List;
init_cashiers({_, Frame, _, _, _, _} = Wx, [], [H|T], CashiersNumber, X,Y, X_Limit, List) ->
    Static_Text = {H, wxStaticText:new(Frame, CashiersNumber, "Send | ", [{pos,{X,Y}}])},
    List2 = lists:append(List, [Static_Text]),
    if 
        X > X_Limit ->
            init_cashiers(Wx,[],T, CashiersNumber,0, Y + 20, X_Limit, List2);
        true ->
            init_cashiers(Wx,[],T, CashiersNumber,X + 60, Y, X_Limit, List2)
    end;
init_cashiers({_, Frame, _, _, _, _} = Wx, [H|T], Send, CashiersNumber, X,Y, X_Limit, List) ->
    Static_Text = {H, wxStaticText:new(Frame, CashiersNumber, "Receive | ", [{pos,{X,Y}}])},
    List2 = lists:append(List, [Static_Text]),
    if 
        X > X_Limit ->
            init_cashiers(Wx,T,Send, CashiersNumber,0, Y + 20, X_Limit, List2);
        true ->
            init_cashiers(Wx,T,Send, CashiersNumber,X + 60, Y, X_Limit, List2)
    end.

init_clients(Clients_Number) ->
    [X || X <- lists:seq(0,Clients_Number)].

show_clients({_, Frame, _, _} = Wx, [H|T], X, Y) ->
    wxStaticText:new(Frame, 0, "C", [{pos,{X,Y}}]),
    timer:sleep(1000),
    show_clients(Wx, T, X, Y + 20);
show_clients(_, [], _, _) -> ok.

clear_clients(F, [H|T], X, Y) ->
    wxStaticText:new(F, 0, "X", [{pos,{X,Y}}]),
    timer:sleep(1000),
    clear_clients(F, T, X, Y + 20);
clear_clients(_, [], _, _) -> ok.

