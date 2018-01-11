-module(cashier_generator).
-compile([export_all]).


generate_cashier() ->
    random:seed(erlang:timestamp()),
    Probability = random:uniform(1000),
    if
        Probability < 200 -> 
            send_package;
        true -> 
            receive_package      
    end.
    