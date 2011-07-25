%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%

%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

%%% Random Generators for several probability distributions

-module(ts_stats).
-created('Date: 2000/10/20 13:58:56 nniclausse Exp ').
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-export([exponential/1, exponential/2, pareto/2,
         normal/0, normal/1, normal/2, uniform/2,
         invgaussian/2,
         mean/1, mean/3,
         variance/1,
         meanvar/4,
         meanvar_minmax/6,
         stdvar/1]).

-import(math, [log/1, pi/0, sqrt/1, pow/2]).

-record(pareto, {a = 1 , beta}).
-record(normal, {mean = 0 , stddev= 1 }).
-record(invgaussian, {mu , lambda}).

%% get n samples from a function F with parameter Param
sample (F, Param, N) ->
    sample(F, [], Param, N-1).

sample (F, X, Param, 0) ->
    [F(Param) | X] ;
sample (F, X, Param, N) ->
    sample(F, [F(Param)|X], Param, N-1 ).

uniform(Min,Max)->
    Min+random:uniform(Max-Min).

%% random sample from an exponential distribution
exponential(Param) ->
    -math:log(random:uniform())/Param.

%% N samples from an exponential distribution
exponential(Param, N) ->
    sample(fun(X) -> exponential(X) end , Param, N).

%% random sample from a Pareto distribution
pareto(#pareto{a=A, beta=Beta}) ->
    A/(math:pow(random:uniform(), 1/Beta)).

%% if a list is given, construct a record for the parameters
pareto([A, Beta], N) ->
    pareto(#pareto{a = A , beta = Beta }, N);
%% N samples from a Pareto distribution
pareto(Param, N) ->
    sample(fun(X) -> pareto(X) end , Param, N).

invgaussian([Mu,Lambda],N) ->
    invgaussian(#invgaussian{mu=Mu,lambda=Lambda},N);

invgaussian(Param,N) ->
    sample(fun(X) -> invgaussian(X) end , Param, N).

%% random sample from a Inverse Gaussian distribution
invgaussian(#invgaussian{mu=Mu, lambda=Lambda}) ->
    Y = Mu*pow(normal(), 2),
    X1 = Mu+Mu*Y/(2*Lambda)-Mu*sqrt(4*Lambda*Y+pow(Y,2))/(2*Lambda),
    U = random:uniform(),
    X = (Mu/(Mu+X1))-U,
    case X >=0 of
        true  -> X1;
        false -> Mu*Mu/X1
    end.

normal() ->
    [Val] = normal(#normal{},1),
    Val.

normal([Mean,StdDev],N) ->
    normal(#normal{mean=Mean,stddev=StdDev},N);

normal(Param,N) ->
    sample(fun(X) -> normal(X) end , Param, N).

normal(N) when is_integer(N)->
    normal(#normal{},N);
normal(#normal{mean=M,stddev=S}) ->
    normal_boxm(M,S,0,0,1).

%%% use the polar form of the Box-Muller transformation
normal_boxm(M,S,X1,_X2,W) when W < 1->
    W2 = sqrt( (-2.0 * log( W ) ) / W ),
    Y1 = X1 * W2,
    M + Y1 * S;
normal_boxm(M,S,_,_,_W) ->
    X1 = 2.0 * random:uniform() - 1.0,
    X2 = 2.0 * random:uniform() - 1.0,
    normal_boxm(M,S,X1,X2,X1 * X1 + X2 * X2).
%%%

%% incremental computation of the mean
mean(Esp, [], _) -> Esp;

mean(Esp, [X|H], I) ->
    Next = I+1,
    mean((Esp+(X-Esp)/(Next)), H, Next).

%% compute the mean of a list
mean([]) -> 0;

mean(H) ->
    mean(0, H, 0).

%% @spec meanvar(Esp::number(),Var::number(),X::list() | number(),I::integer()) ->
%%      {NewEsp::number(), NewVar::number(), Next::integer()}
%% @doc incremental computation of the mean and variance together. The
%%      algorithm should limit the round-off errors
%% @end

%% single value
meanvar(Esp, Var, X, I) when is_number(X) ->
    Next = I+1,
    C = X - Esp,
    NewEsp =  (X+Esp*I)/(Next),
    NewVar = Var+C*(X-NewEsp),
    { NewEsp, NewVar, Next };
%% list of samples
meanvar(Esp, Var,[], I) ->
    {Esp, Var, I};
meanvar(Esp, Var, [X|H], I) ->
    {NewEsp, NewVar, Next} = meanvar(Esp,Var,X,I),
    meanvar(NewEsp, NewVar, H, Next).

%% compute min and max also
meanvar_minmax(Esp, Var, Min, Max, X, I) when is_number(X)->
    meanvar_minmax(Esp, Var, Min, Max, [X], I);
meanvar_minmax(Esp, Var, Min, Max, [], I) ->
    {Esp, Var, Min, Max, I};
meanvar_minmax(Esp, Var, 0, 0, [X|H], I) -> % first data, set min and max
    meanvar_minmax(Esp, Var, X, X, [X|H], I);
meanvar_minmax(Esp, Var, Min, Max, [X|H], I) ->
    {NewEsp, NewVar, Next} = meanvar(Esp,Var,X,I),
    if
        X > Max -> % new max, min unchanged
            meanvar_minmax(NewEsp, NewVar, Min, X, H, Next);
        X < Min -> % new min, max unchanged
            meanvar_minmax(NewEsp, NewVar, X, Max, H, Next);
        true ->
            meanvar_minmax(NewEsp, NewVar, Min, Max, H, Next)
    end.


%% compute the variance of a list
variance([]) -> 0;
variance(H) ->
    {_Mean, Var, I} = meanvar(0, 0, H, 0),
    Var/I.

stdvar(H) ->
    math:sqrt(variance(H)).


