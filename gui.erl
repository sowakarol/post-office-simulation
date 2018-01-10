-module(gui).
-compile([export_all]).

initFrame(CashiersNumber) ->
    Wx = wx:new(),
    %TODO config size X,Y

    F = wxFrame:new(Wx, -1, "Hallo!", [{size,{1000,500}}]),
    wxFrame:createStatusBar(F),
    wxFrame:setStatusText(F, "Quiet here."), 
    wxFrame:show(F),
    SB = wxFrame:getStatusBar(F),
    wxStatusBar:pushStatusText(SB, "A LITTLE LOUDER NOW."),
    wxStatusBar:popStatusText(SB), 
    initCashiers(F,CashiersNumber,0,0,980),
    F.

initCashiers(_, 0,_,_,_) -> ok;

initCashiers(F, CashiersNumber, X,Y, X_Limit) ->
    wxStaticText:new(F, CashiersNumber, "X", [{pos,{X,Y}}]),
    if 
        X > X_Limit ->
            initCashiers(F,CashiersNumber - 1, 0, Y + 20, X_Limit);
        true ->
            initCashiers(F,CashiersNumber - 1, X + 20, Y, X_Limit)
    end.



terminate(Frame) ->
    wxFrame:destroy(Frame).

