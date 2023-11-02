:-lib(gfd).

%assignment_csp(NP, MT, ASP, ASA)
% NP πλήθος των διαθέσιμων ατόμων 
% MT μέγιστος συνολικός χρόνος των δραστηριοτήτων που μπορεί να αναλάβει ένα άτομο
% ASP =  [N-As-T|...], N είναι ο αύξων αριθμός ατόμου (1, 2, …, NP), As λίστα των δραστηριοτήτων, T είναι η συνολική διάρκεια
% ASA =  [A-N|...], η δραστηριότητα A ανατίθεται στο άτομο N

%assignment_csp(3, 14, ASP, ASA).
%ASP = [1 - [a15, a12, a09, a07, a05, a03] - 13,
% 2 - [a14, a11, a08, a04, a01] - 14,
% 3 - [a13, a10, a06, a02] - 12]

%ASA = [a01 - 2, a02 - 3, a03 - 1, a04 - 2, a05 - 1, a06 - 3,
% a07 - 1, a08 - 2, a09 - 1, a10 - 3, a11 - 2, a12 - 1,
% a13 - 3, a14 - 2, a15 - 1]

index(_, [], _):-fail.
index(1,[Element|_],Element).
index(Index,[_|List],Element):-
    Index0 #= Index - 1,
    index(Index0,List,Element).

assignment_csp(NumberOfWorkers,MaxTime,ListOLists):-
    findall(activity(Name,act(Start,End)),activity(Name,act(Start,End)),AllActivities),
    length(ListOLists,NumberOfWorkers),
    assign_activities_to_workers(MaxTime,ListOLists,AllActivities),
    flatten(ListOLists,Flat),
    alldifferent(Flat),
    length(AllActivities,NumberOfAllActivities),
    length(Flat,NumberOfAllActivities),
    search(Flat,0,input_order,indomain,complete,[]).

assign_activities_to_workers(_,[],_).
assign_activities_to_workers(MaxTime,[List|ListOLists],AllActivities):-
    valid_activity_lists(List,MaxTime,AllActivities),
    assign_activities_to_workers(MaxTime,ListOLists,AllActivities).

% length(List_Activities,10),findall(N,activity(N,act(X,Y)),L),length(L,Length) ,List_Activities :: 0..Length,gfd:labeling(List_Activities).
valid_activity_lists(ActivityIndexList,MaxTime,AllActivities):-
    % get all the activities in a list, might do that part somewhere else in code and just pass it as info later
    %findall(activity(Name,act(Start,End)),activity(Name,act(Start,End)),AllActivities),
    % get the domain for the number of activities
    length(AllActivities,NumberOfAllActivities),
    ActivityListSize :: 0..NumberOfAllActivities,
    % get the domain for the actual indexes of the activities
    length(ActivityIndexList,ActivityListSize),
    ActivityIndexList :: 1..NumberOfAllActivities,
    % constraints
    alldifferent(ActivityIndexList),
    activity_list_constraints(ActivityIndexList,AllActivities,MaxTime),
    search(ActivityIndexList,0,most_constrained,indomain,complete,[]).

activity_list_constraints([],[]):-!.
activity_list_constraints([ActivityIndex],AllActivities,MaxTime):-
    index(ActivityIndex,AllActivities,activity(_,act(ActivityStart,ActivityEnd))),
    Time #= ActivityEnd - ActivityStart,
    MaxTime #>= Time,!.
activity_list_constraints([ActivityIndex1,ActivityIndex2|ActivityIndexList],AllActivities,MaxTime):-
    MaxTime #>=0,
    index(ActivityIndex1,AllActivities,activity(_,act(ActivityStart1,ActivityEnd1))),
    index(ActivityIndex2,AllActivities,activity(_,act(ActivityStart2,_))),
    ActivityStart2 #> ActivityEnd1,
    Time #= ActivityEnd1 - ActivityStart1,
    RemainingTime #= MaxTime - Time,
    activity_list_constraints([ActivityIndex2|ActivityIndexList],AllActivities,RemainingTime),!.
