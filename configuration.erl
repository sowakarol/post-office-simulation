-module(configuration).
-compile([export_all]).

% post office configuration
number_of_cashier_receiving()   -> 3.   % their duty is to handle clients that want to receive a package MIN 1
number_of_cashier_sending()     -> 2.   % their duty is to handle clients that want to send a package MIN 1
working_hours_start()           -> 9.   % 09:00 MIN 0 MAX 23 
working_hours_end()             -> 10.  % 10:00 MIN 0 MAX 24
days()                          -> 2.   % how many days post office will be simulated % MIN 0
break_after_day()               -> 10.  % in seconds % MIN 0

% cashier configuration - time in MINUTES
average_time_sending_package()      -> 3. % MIN 0
average_time_receiving_package()    -> 4. % MIN 0
%average_time_answering_question()   -> 1.


% simulation time
one_minute_in_application() -> 0.2. % in seconds % MIN > 0

%consumers configurations
% average number IN ONE HOUR
average_consumers_per_night()       -> 25.  %% 00:00 -  6:00 % MIN 0 
average_consumers_per_morning()     -> 300. %%  6:00 - 11:00 % MIN 0 
average_consumers_per_noon()        -> 55.  %% 11:00 - 14:00 % MIN 0
average_consumers_per_afternoon()   -> 75.  %% 14:00 - 19:00 % MIN 0
average_consumers_per_evening()     -> 50.  %% 19:00 - 24:00 % MIN 0


%hesitation's values of random generator in percents
percentage()    -> 10. % MIN 0 MAX 100

%%
working_hours_start_min()       -> round(working_hours_start() * 60).   % 09:00 
working_hours_end_min()         -> round(working_hours_end() * 60).
