-module(ea).
-export([start/0]).
-define(CORRECT, "ABCDEFGHIJKLMNOPQRSTUVWXYZ").
-define(POPSIZE, 1000). %% Size of our population for each tournament
-define(TOURNAMENT_SIZE, 10).
-define(COUNTER_TABLE,generation_counter).

start() -> % Fire this guy off to start the tournaments for each population.
	ok = create_generation_counter(),
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
	Pop = create_populations(?POPSIZE),
	run(Pop).

run(Pop) -> %first run of tournaments (no new generations)
	Tournament = create_tournament(Pop),
	case run_tournament(Tournament) of
		[ChildA,ChildB] ->
			ChildPop = [ChildA,ChildB],
			run(Pop,ChildPop);
		ok ->
			ok
	end.

run(Pop,ChildPop) -> %Accumulate children until we have enough to replace our parents
	Tournament = create_tournament(Pop),
	case run_tournament(Tournament) of
		[ChildA,ChildB] ->
			NewChildPop = [ChildA,ChildB],
			NewPop = lists:append(ChildPop,NewChildPop),
			case length(NewPop) =:= ?POPSIZE of
				true ->
					ok = increment_generation_counter(),
					run(NewPop);
				false ->
					run(Pop,NewPop)
			end;
		ok ->
			ok
	end.

%% Population Creation
create_populations(PopSize) ->
	gen_pop([],PopSize).

gen_pop(Pops,0) ->
	Pops;
gen_pop(Pops,N) ->
	NewPops = lists:append(Pops,[fill(length(?CORRECT))]),
	gen_pop(NewPops,N-1).

fill(N) -> % Fill in our "individual" with their own genome from our "perfect" one.
	fill([],N).

fill(P,0) ->
	P;
fill(P,N) ->
	Pos = random:uniform(length(?CORRECT)),
	E = lists:nth(Pos,?CORRECT),
	NP = P ++ [E],
	fill(NP,N-1).

%% Fitness
%% Here we keep score of how "fit" our individual is...
%% How well do they measure up to our "perfect" genome?
population_fitness(Pops) ->
	Results = get_fitness(Pops,[]),
	lists:reverse(lists:keysort(2,Results)).

get_fitness([], Result) ->
	Result;
get_fitness([PH|PT], Result) when is_list(Result) ->
	{Person,Score} = fit(PH),
	NewResult = lists:append(Result,[{Person,Score}]),
	get_fitness(PT,NewResult).

fit(L) ->
	fit(?CORRECT,L,0,L).

fit([],[],Score,Orig) -> 
	{Orig,Score};
fit([AH|AT],[BH|BT],Score,Orig) ->
	NewScore = case AH =:= BH of
		true ->
			Score + 1;
		false ->
			Score
	end,
	fit(AT,BT,NewScore,Orig).

%% Tournament Creation
create_tournament([]) ->
	Pop = create_populations(?POPSIZE),
	create_tournament(Pop);

create_tournament(Pop) ->
	seed_tournament(Pop,[],?TOURNAMENT_SIZE).

seed_tournament(_Orig,AccPop,0) ->
	AccPop;

seed_tournament(OrigPop,AccPop,N) ->
	Pos = random:uniform(length(OrigPop)),
	SelectedIndividual = lists:nth(Pos,OrigPop),
	NewPop = lists:append(AccPop,[SelectedIndividual]),
	seed_tournament(OrigPop,NewPop,N-1).
	
%% If we are not given a population, we will generate one,
%% otherwise, we will use the one passed in (the children we just generated)
run_tournament(Individuals) ->
	Results = population_fitness(Individuals), % How fit is our population
	{Winner,_WinningScore} = hd(Results), % Get the alpha of our population
	{RunnerUp,_RunnerUpScore} = lists:nth(2,Results), % Get the second alpha
	% io:format("Best of Tournament: ~p Score: ~p~n",[Winner,WinningScore]),
	[{a,ChildA},{b,ChildB}] = crossover(Winner,RunnerUp), % Breed them together
	[MutatedChildA, MutatedChildB] = [mutate(ChildA),mutate(ChildB)],
	case criteria_check(MutatedChildA, MutatedChildB) of
		true ->
			ok = get_generation_counter(),
			% io:format("ChildA: ~p~nChildB: ~p~n",[MutatedChildA,MutatedChildB]),
			ok = destroy_generation_counter(),
			ok;
		false ->
			[MutatedChildA, MutatedChildB]
	end.

%% Crossover and Mutation functions
%% This is a single point crossover.  You can read more on crossover's here:
%% http://www.obitko.com/tutorials/genetic-algorithms/crossover-mutation.php
crossover(ParentA,ParentB) ->
	case random:uniform(100) =< 80 of %only occur 80% of the time
		true ->
			COPoint = random:uniform(length(?CORRECT)),
			ChildA = lists:sublist(ParentA,COPoint) ++ lists:sublist(ParentB,COPoint+1,length(ParentB)),
			ChildB = lists:sublist(ParentB,COPoint) ++ lists:sublist(ParentA,COPoint+1,length(ParentA));
		false ->
			ChildA = ParentA,
			ChildB = ParentB
	end,
	[{a, ChildA},{b, ChildB}].

% All we mutate with here is a single random element in our sequence.
mutate(Child) ->
	case random:uniform(100) =< 5 of % 5% chance of mutation.
		true ->
			MP = random:uniform(length(Child)), %Mutation Point (MP)
			lists:sublist(Child,MP - 1) ++ [lists:nth(random:uniform(length(?CORRECT)),?CORRECT)] ++ lists:sublist(Child,MP+1,length(Child));
		false ->
			Child
	end.

%% Criteria Check to halt on.
%% Here we check to see if we've hit what we think of as "complete".
%% You'll notice on the AlphaLimit that it is set to the size of ?CORRECT.
%% We finish if either Child A or Child B are good enough for us to stop.
%% So if they are 26 (assuming you haven't changed ?CORRECT) or better, 
%% we'll stop and see what our near-perfect genome is.
%% fool around with this (set it lower).
criteria_check(ChildA,ChildB) -> %%This could be a lot better!!!
	{CA,ChildAFitness} = fit(ChildA),
	{CB,ChildBFitness} = fit(ChildB),
	AlphaLimit = 26,
	case ChildAFitness >= AlphaLimit of
		true ->
			io:format("A: ~p scored: ~p.~n",[CA,ChildAFitness]),
			true;
	 	false->
			case ChildBFitness >= AlphaLimit of
				true ->
					io:format("B: ~p scored: ~p.~n",[CB,ChildBFitness]),
					true;
				false ->
					false
			end
	end.

%% Generation Counter
create_generation_counter() ->
	case ets:new(?COUNTER_TABLE, [set,public,named_table]) of
		Name when is_atom(Name) -> 
			ets:insert(?COUNTER_TABLE,{generation,0}),
			io:format("Success! Created counter table.~n",[]);
		_ ->
			io:format("Warning! There was a problem creating the ets table!~n",[])
	end,
ok.

increment_generation_counter() ->
	CurrentGen = ets:update_counter(?COUNTER_TABLE,generation,1),
	io:format("Current generation is: ~p.~n",[CurrentGen]),
	ok.

get_generation_counter() ->
	[{_,Gen}] = ets:lookup(?COUNTER_TABLE,generation),
	io:format("**FINAL GENERATION** WAS: ~p.~n",[Gen]),
	ok.

destroy_generation_counter() ->
	case ets:delete(?COUNTER_TABLE) of
		true ->
			io:format("Deleted generation counter table!~n",[]);
		false ->
			io:format("There was a problem deleting the generation table!",[])
	end,
	ok.