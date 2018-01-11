-module(client_generator).
-compile([export_all]).


generate_client() ->
    random:seed(erlang:timestamp()),
    Probability = random:uniform(1000),
    if
        Probability < 200 -> 
            RandomTime = random_data:generate(configuration:average_time_sending_package()),
            {client, send_package, RandomTime};
        Probability < 800 -> 
            RandomTime = random_data:generate(configuration:average_time_receiving_package()),
            {client, receive_package, RandomTime};
        true -> 
            RandomTime = random_data:generate(configuration:average_time_answering_question()),
            {client, ask_question, RandomTime}     
    end.
