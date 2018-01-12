-module(gui).
-compile([export_all]).
-include_lib("wx/include/wx.hrl").

init(Cashiers_Number) ->
    Wx = make_window(),
    Cashiers = init_cashiers(Wx, Cashiers_Number,0,0,980,[]),
    % show_clients(Wx, init_clients(10), 0, 100),
    loop(Wx, Cashiers).

make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Post office simulation", [{size,{1000,500}}]),
    End_Button = wxButton:new(Frame, ?wxID_STOP, [{label, "End simulation"}, {pos, {300,10}}]),
    
    wxFrame:createStatusBar(Frame),
    wxFrame:show(Frame),
    
    wxFrame:connect(Frame, close_window),
    wxButton:connect(End_Button, command_button_clicked),

    {Server, Frame, End_Button}.

loop(Wx, Cashiers) ->
    {Server, Frame, End_Button} = Wx,
    io:format("--waiting in the loop--~n", []),
    receive 
        #wx{id = ?wxID_STOP, event=#wxCommand{type = command_button_clicked}} ->
            io:format("--ending simulation-- ~n"), 
            destroy_cashiers(Cashiers),
            wxWindow:destroy(End_Button),
            wxFrame:setStatusText(Frame, "Simulation ended."), 
            end_simulation(Frame);
        #wx{event=#wxClose{}} ->
            io:format("--closing window ~p-- ~n",[self()]),
            wxWindow:destroy(Frame),
            ok
    end.

end_simulation(Frame) ->
    io:format("--simulation ended--~n"),
    receive
        #wx{event=#wxClose{}} ->
            io:format("--closing window ~p-- ~n",[self()]),
            wxWindow:destroy(Frame),
            ok
    end.

destroy_cashiers(List) ->
    lists:foreach(fun(H) -> wxWindow:destroy(H) end, List).

init_cashiers(_, 0,_,_,_, List) -> List;
init_cashiers({_, Frame, _} = Wx, CashiersNumber, X,Y, X_Limit, List) ->
    List2 = lists:append(List, [wxStaticText:new(Frame, CashiersNumber, "X", [{pos,{X,Y}}])]),
    if 
        X > X_Limit ->
            init_cashiers(Wx,CashiersNumber - 1, 0, Y + 20, X_Limit, List2);
        true ->
            init_cashiers(Wx,CashiersNumber - 1, X + 20, Y, X_Limit, List2)
    end.

init_clients(Clients_Number) ->
    [X || X <- lists:seq(0,Clients_Number)].

show_clients({_, Frame, _} = Wx, [H|T], X, Y) ->
    wxStaticText:new(Frame, 0, "C", [{pos,{X,Y}}]),
    % timer:sleep(1000),
    show_clients(Wx, T, X, Y + 20);
show_clients(_, [], _, _) -> ok.

clear_clients(F, [H|T], X, Y) ->
    wxStaticText:new(F, 0, "X", [{pos,{X,Y}}]),
    timer:sleep(1000),
    clear_clients(F, T, X, Y + 20);
clear_clients(_, [], _, _) -> ok.

