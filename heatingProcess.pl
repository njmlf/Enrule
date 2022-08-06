/* 冷却方式 */
cooling_type(oil).                                          /* 油冷 */
cooling_type(water).                                        /* 水冷 */
cooling_type(furnace).                                      /* 炉冷 */
cooling_type(air).                                          /* 空冷 */

/* 热处理方式 */
treatment_type(tempering).                                  /* 回火 */
treatment_type(hardening).                                  /* 淬火 */
treatment_type(annealing).                                  /* 退火 */
treatment_type(spheroidizing_annealing).                    /* 球化退火 */
treatment_type(aging).                                      /* 时效处理 */

/* 热处理炉 */
furnace_type(tempering_furnace).                            /* 回火炉 */
furnace_type(hardening_furnace).                            /* 淬火炉 */
furnace_type(annealing_furnace).                            /* 退火炉 */

/* 上料台 */

/* 工艺路线 */
% heat_treating(TreatmentType, Temperature, SoakingTime, CoolingType).        /* 热处理(热处理方式，加热温度, 保温时间, 冷却方式) */
% routing(Seq, Name, HeatTreatings).                                          /* 工艺路线（编号，名称，热处理列表）*/
% predefined_routings(Routings).                                              /* 预定义工艺路线 */

routing(1, 'r1', ht1).
routing(2, 'r2', ht2).
routing(3, 'r3', ht3).

/* 热处理 */
process(Routine) :- 
    routing(Routine, Name, HeatTreatings),
    format(Name),
    format(HeatTreatings).
    %!.

can

move(s_system_stoped, system_power_on, s_system_ready).
move(_, system_power_off, s_system_stoped).
move()




/* '油冷'(Duration).
'淬火'(Temperature, HeatingDuration, CoolingType).
'工艺路线'([]).
'工艺路线'(b, 2).*/

move(state(middle,onbox,middle,hasnot),
   grasp,
   state(middle,onbox,middle,has)).
move(state(P,onfloor,P,H),
   climb,
   state(P,onbox,P,H)).
move(state(P1,onfloor,P1,H),
   drag(P1,P2),
   state(P2,onfloor,P2,H)).
move(state(P1,onfloor,B,H),
   walk(P1,P2),
   state(P2,onfloor,B,H)).
canget(state(_,_,_,has)).
canget(State1) :-
   move(State1,_,State2),
   canget(State2).

arc(1,2).
arc(1,3).
arc(2,4).
arc(2,5).
arc(2,6).
arc(5,7).
arc(3,8).
arc(3,9).
arc(9,10).

% path_leaf(N,P) <- P is a path starting at node N, ending
% in a leaf in the graph given by arc/2
path_leaf(Leaf, [Leaf]) :- 
    leaf(Leaf). 

path_leaf(Node1,[Node1|Nodes]):-
    arc(Node1,Node2),
    path_leaf(Node2,Nodes).

leaf(Leaf):- not(arc(Leaf, SomeNode)).
