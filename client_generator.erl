-module(client_generator).
-export([generate_client/0,generate_clients_number/1]).

generate_client() ->
    random:seed(erlang:timestamp()),
    Probability = random:uniform(1000),
    if
        Probability < 0 ->
            throw(non_positive_probability);
        Probability < 200 -> 
            RandomTime = random_data:generate(configuration:average_time_sending_package()),
            {client, send_package, RandomTime};
        true ->
            RandomTime = random_data:generate(configuration:average_time_receiving_package()),
            {client, receive_package, RandomTime}


        % third case scenario
        % true -> 
        %     RandomTime = random_data:generate(configuration:average_time_answering_question()),
        %     {client, ask_question, RandomTime}     
    
    end.


%hour argument is passed in minutes
generate_clients_number(DayTimeInMinutes) ->
    if
        DayTimeInMinutes < 0 -> 
            throw(incorrect_day_time);
        DayTimeInMinutes < 360 -> % 6:00
            generate_clients_number_for_minute(configuration:average_consumers_per_night()/6);
        DayTimeInMinutes < 660 -> % 11:00
            generate_clients_number_for_minute(configuration:average_consumers_per_morning()/5);
        DayTimeInMinutes < 840 -> % 14:00
            generate_clients_number_for_minute(configuration:average_consumers_per_noon()/3);
        DayTimeInMinutes < 1140 -> % 19:00
            generate_clients_number_for_minute(configuration:average_consumers_per_afternoon()/5); 
        DayTimeInMinutes < 1440 -> % 18:00
            generate_clients_number_for_minute(configuration:average_consumers_per_evening()/6);
        true -> throw(incorrect_day_time)
    end.

generate_clients_number_for_minute(Number) ->
    Num = Number/60,
    if 
        Num >= 1.0 -> round(Num);
        true -> Chance = Num * 100,
                N = random:uniform(100),
                if 
                    Chance >= N -> 1;
                    true -> 0
                end
    end.
    

