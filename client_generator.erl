-module(client_generator).
-export([generate_client/0,generate_clients_number/2]).

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
generate_clients_number(DayTimeInMinutes, Interval) ->
    if
        DayTimeInMinutes < 0 -> 
            throw(incorrect_day_time);
        DayTimeInMinutes < 360 -> % 6:00
            round(configuration:average_consumers_per_night()/Interval);
        DayTimeInMinutes < 660 -> % 11:00
            round(configuration:average_consumers_per_morning()/Interval);
        DayTimeInMinutes < 840 -> % 14:00
            round(configuration:average_consumers_per_noon()/Interval);
        DayTimeInMinutes < 1140 -> % 19:00
            round(configuration:average_consumers_per_afternoon()/Interval); 
        DayTimeInMinutes < 1440 -> % 18:00
            round(configuration:average_consumers_per_evening()/Interval);
        true -> throw(incorrect_day_time)
    end.

