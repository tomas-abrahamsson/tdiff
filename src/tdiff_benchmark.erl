%%% Copyright (C) 2010-2011  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Library General Public
%%% License as published by the Free Software Foundation; either
%%% version 2 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Library General Public License for more details.
%%%
%%% You should have received a copy of the GNU Library General Public
%%% License along with this library; if not, write to the Free
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

-module(tdiff_benchmark).
-import(lists, [map/2, foreach/2]).
-export([start/0, start/1]).
-compile(export_all).

-record(telem, {sz        :: integer(),
		variation :: float(), %% 0..1
		s1        :: string(),
		s2        :: string()}).


start() ->
    start(_Opts=[]).

start(Opts) ->
    Seed = proplists:get_value(random_seed, Opts, random:seed0()),
    set_random_seed(Seed),
    Lengths = map(fun power_of_ten/1, lists:seq(1,3)),
    OrigStrs = map(fun create_string_of_len/1, Lengths),
    Changes = lists:append(
		map(fun(OrigStr) ->
			    lists:append(
			      map(fun(V) ->
					  map(fun(_) ->
						      create_test_elem(
							OrigStr, V)
					      end,
					      lists:seq(1,_NumPairs=3))
				  end,
				  _Variations=[0.05, 0.25, 0.50, 0.75]))
		    end,
		    OrigStrs)),

    run_tdiff_bm(Changes).


power_of_ten(0) -> 1;
power_of_ten(N) -> 10 * power_of_ten(N-1).

create_string_of_len(N) -> lists:duplicate(N, $x).

create_test_elem(Str, DegreeOfVariation) ->
    S1 = create_variation(Str, DegreeOfVariation),
    S2 = create_variation(Str, DegreeOfVariation),
    #telem{sz        = length(Str),
	   variation = DegreeOfVariation,
	   s1        = S1,
	   s2        = S2}.

create_variation("", _ChangePercent) ->
    "";
create_variation(Str, ChangePercent) ->
    NumChangesPerStr = length(Str) * ChangePercent,
    VariationChancePerChar = NumChangesPerStr / length(Str),
    c_v_2(Str, VariationChancePerChar).

c_v_2([C|T]=Str, VariationChancePerChar) ->
    DiceRoll = random:uniform(),
    if DiceRoll < VariationChancePerChar ->
	    case get_random_change() of
		delete      -> c_v_2(T, VariationChancePerChar);
		{insert,C2} -> [C2 | c_v_2(Str, VariationChancePerChar)];
		{change,C2} -> [C2 | c_v_2(T, VariationChancePerChar)]
	    end;
       true ->
	    [C | c_v_2(T, VariationChancePerChar)]
    end;
c_v_2("", _VariationChancePerChar) ->
    "".

get_random_change() ->
    case random:uniform(3) of
	1 -> delete;
	2 -> {insert, get_random_char()};
	3 -> {change, get_random_char()}
    end.

get_random_char() -> $a + random:uniform(26) - 1.


run_tdiff_bm(TestElems) ->
    NumRuns = 10,
    print_results(
      lists:sort(
	dict:to_list(
	  lists:foldl(fun(#telem{sz=Size,variation=V,s1=S1,s2=S2}, D) ->
			      {US,GC} = run_num_times(NumRuns,
                                                      fun tdiff:diff/2,
                                                      [S1,S2]),
			      Key = {Size,V},
			      {AccTime, AccGC, N} = case dict:find(Key, D) of
                                                        {ok,X} -> X;
                                                        error  -> {0,0,0}
                                                    end,
                              NewValue = {AccTime+US, AccGC+GC, N+NumRuns},
			      dict:store(Key, NewValue, D)
		      end,
		      dict:new(),
		      TestElems)))).

print_results(Results) ->
    io:format(" size    variation    avg time      avg garbage~n"),
    WordSize = erlang:system_info(wordsize),
    foreach(
      fun({{Size, Variation}, {US,GC,N}}) ->
              if GC < 0 -> io:format("*** GC counter wrapped!~n"
                                     "*** Please rerun the test!~n");
                 true   -> ok
              end,
              io:format("~6w     ~.2f     ~10s  ~15s~n",
                        [Size, Variation,
                         micros_to_pretty_time(round(US/N)),
                         bytes_to_pretty_size(round(GC*WordSize/N))])
      end,
      Results),
    Results.

micros_to_pretty_time(US) when US >= 1000*1000 ->
    f("~.2f s", [US/(1000*1000)]);
micros_to_pretty_time(US) when US >= 1000 ->
    f("~.2f ms", [US/1000]);
micros_to_pretty_time(US) ->
    f("~w us", [US]).

bytes_to_pretty_size(N) when N >= 1024*1024 ->
    f("~.2f MB", [N / (1024*1024)]);
bytes_to_pretty_size(N) when N >= 1024 ->
    f("~.2f kB", [N / 1024]);
bytes_to_pretty_size(N) ->
    f("~w bytes", [N]).

f(F,A) -> lists:flatten(io_lib:format(F,A)).

set_random_seed({A,B,C}) ->
    random:seed(A,B,C).

run_num_times(N, Fun, Args) ->
    run_num_times_2(N, Fun, Args, {0,0}).

run_num_times_2(N, Fun, Args, {AccTime, AccGC}) when N > 0 ->
    {US, GC} = run_once(Fun,Args),
    run_num_times_2(N-1, Fun, Args, {AccTime+US, AccGC+GC});
run_num_times_2(0, _Fun, _Args, Acc) ->
    Acc.

run_once(Fun, Args) ->
    Master = self(),
    P = spawn(fun() ->
                      Ps = processes(),
                      [garbage_collect(P) || P <- Ps],
                      [garbage_collect(P) || P <- Ps],
                      {_,Reclaimed0,_} = erlang:statistics(garbage_collection),
                      {US, _Res} = timer:tc(erlang, apply, [Fun,Args]),
                      garbage_collect(),
                      garbage_collect(),
                      garbage_collect(),
                      {_,Reclaimed1,_} = erlang:statistics(garbage_collection),
                      GC = Reclaimed1 - Reclaimed0,
                      if GC < 0 ->
                              io:format("Reclaimed0=~w~nReclaimed1=~w~n",
                                        [Reclaimed0, Reclaimed1]);
                         true ->
                              ok
                      end,
                      Master ! {self(), {US, GC}}
              end),
    receive
        {P, Data} -> Data
    end.
