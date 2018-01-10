-module(random_data).
-export([generate/1]).


% to add to average data some possible delays


generate(Average) ->
    random:seed(erlang:timestamp()),
    Remainder = round(Average / 10),
    Positive = random:uniform(3),
    case Positive of
        1 -> Average + random:uniform(Remainder);
        2 -> (-1 * random:uniform(Remainder)) + Average;
        _ -> Average
    end.



