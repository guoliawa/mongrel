% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%%% @author CA Meijer
%%% @copyright 2012 CA Meijer
%%% @doc Mongrel API. This module provides functions for creating, reading, updating and deleting
%%       documents. The functions exposed are similar to the CRUD functions exposed by the
%%       mongo API of the MongoDB driver. Typically, the mongo driver functions require both
%%       collection and document arguments. The mongrel functions usually require one fewer
%%       argument since the collection and document contents are both encapsulated in a record.
%%% @end

-module(mongrel).

-behaviour(gen_server).

%% API
-export([count/1,
		 count/2,
		 delete/1,
		 delete_one/1,
		 do/5,
		 find/1,
		 find/2,
		 find/3,
		 find/4,
		 find_one/1,
		 find_one/2,
		 find_one/3,
		 insert/1,
		 insert_all/1,
		 modify/2,
		 replace/2,
		 repsert/2,
		 save/1]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-record(state, {}).

%% External functions

%% @doc Counts the number of documents that match some selector.
%%
%% @spec count(record()) -> integer()
%% @end
count(SelectorRecord) ->
	count(SelectorRecord, 0).

%% @doc Counts the number of documents that match some selector up to a specified
%%      maximum. A limit of 0 means that all matching documents are counted.
%%
%% @spec count(record(), integer()) -> integer()
%% @end
count(SelectorRecord, Limit) ->
	Collection = mongrel_mapper:get_type(SelectorRecord),
	Selector = mongrel_mapper:map_selector(SelectorRecord),
	mongo:count(Collection, Selector, Limit).
	
%% @doc Deletes all documents that match a selector.
%%
%% @spec delete(record()) -> ok
%% @end
delete(SelectorRecord) ->
	Collection = mongrel_mapper:get_type(SelectorRecord),
	Selector = mongrel_mapper:map_selector(SelectorRecord),
	mongo:delete(Collection, Selector).

%% @doc Deletes the first document that matches a selector.
%%
%% @spec delete_one(record()) -> ok
%% @end
delete_one(SelectorRecord) ->
	Collection = mongrel_mapper:get_type(SelectorRecord),
	Selector = mongrel_mapper:map_selector(SelectorRecord),
	mongo:delete_one(Collection, Selector).

%% @doc Executes an 'action' using the specified read and write modes to a database using a connection.
%%      An 'action' is anonymous function that takes no arguments. The fun will usually invoke functions
%%      to do inserts, finds, modifies, deletes, etc.
%%
%% @spec do(mongo:write_mode(), mongo:read_mode(), mongo:connection()|mongo:rs_connection(), mongo:db(), mongo:action()) -> {ok, any()}|{failure, any()}    
%% @end
do(WriteMode, ReadMode, Connection, Database, Action) ->
	%% Since we need to store state information, we spawn a new process for this
	%% function so that if the Action also invokes the 'do' function we don't wind up trashing
	%% the original state.
	{ok, Pid} = gen_server:start_link(?MODULE, [WriteMode, ReadMode, Connection, Database], []),
	gen_server:call(Pid, {do, WriteMode, ReadMode, Connection, Database, Action}, infinity).

%% @doc Finds all documents that match a selector and returns a cursor.
%%
%% @spec find(record()) -> cursor()
%% @end
find(SelectorRecord) ->
	find(SelectorRecord, []).

%% @doc Finds all documents that match a selector and returns a cursor
%%      of a projection result. The projection can be passed as a mapped
%%      record or as a Mongo tuple consisting of alternating keys and values.
%%      An empty list ([]) indicates that the full projection of documents must
%%      be returned.
%%
%% @spec find(record(), record() | tuple()) -> cursor()
%% @end
find(SelectorRecord, ProjectorRecord) ->
	find(SelectorRecord, ProjectorRecord, 0).

%% @doc Finds all documents that match a selector and returns a cursor
%%      of a projection result that skips a specified number of matching
%%      documents. 
%%
%% @spec find(record(), record() | tuple(), integer()) -> cursor()
%% @end
find(SelectorRecord, ProjectorRecord, Skip) ->
	find(SelectorRecord, ProjectorRecord, Skip, 0).

%% @doc Finds all documents that match a selector and returns a cursor
%%      of a projection result that skips a specified number of matching
%%      documents.  The cursor retrieves results in the specified batch size.
%%
%% @spec find(record(), record() | tuple(), integer(), integer()) -> cursor()
%% @end
find(SelectorRecord, ProjectorRecord, Skip, BatchSize) ->
	Collection = mongrel_mapper:get_type(SelectorRecord),
	Selector = mongrel_mapper:map_selector(SelectorRecord),
	Projector = mongrel_mapper:map_projection(ProjectorRecord),
	MongoCursor = mongo:find(Collection, Selector, Projector, Skip, BatchSize),
	WriteMode = get(write_mode),
	ReadMode = get(read_mode),
	Connection = get(connection),
	Database = get(database),
	mongrel_cursor:cursor(MongoCursor, WriteMode, ReadMode, Connection, Database, Collection).

%% @doc Finds the first document that matches a selector and returns the document.
%%
%% @spec find_one(record()) -> record()
%% @end
find_one(SelectorRecord) ->
	find_one(SelectorRecord, []).

%% @doc Finds the first document that matches a selector and returns a
%%      projection of the document. The empty projection ([]) means
%%      that all fields in the document are populated.  The projection can be 
%%      passed as a mapped record or as a Mongo tuple consisting of alternating 
%%      keys and values.
%%
%% @spec find_one(record(), record() | tuple()) -> record()
%% @end
find_one(SelectorRecord, ProjectorRecord) ->
	find_one(SelectorRecord, ProjectorRecord, 0).

%% @doc Finds a document that matches a selector and returns a
%%      projection of the document after skipping a certain number of 
%%      matching documents.
%%
%% @spec find_one(record(), record() | tuple(), integer()) -> record()
%% @end
find_one(SelectorRecord, ProjectorRecord, Skip) ->
	Collection = mongrel_mapper:get_type(SelectorRecord),
	Selector = mongrel_mapper:map_selector(SelectorRecord),
	Projector = mongrel_mapper:map_projection(ProjectorRecord),
	{Res} = mongo:find_one(Collection, Selector, Projector, Skip),
	CallbackFunc = fun(Coll, Id) ->
						   {Reference} = mongo:find_one(Coll, {'_id', Id}),
						   Reference
				   end,
	{mongrel_mapper:unmap(Collection, Res, CallbackFunc)}.

%% @doc Inserts a record into a collection with the same name as the record type. If the 
%%      record contains nested records with '_id' fields, the nested documents are upserted
%%      into their appropriate collections as well. The ID of the inserted document is
%%      returned.
%%
%% @spec insert(record()) -> bson:value()
%% @end
insert(Record) ->
	{{Collection, Document}, ChildDocuments} = mongrel_mapper:map(Record),
	[mongo:save(ChildCollection, ChildDocument) || {ChildCollection, ChildDocument} <- ChildDocuments],
	mongo:insert(Collection, Document).

%% @doc Inserts a list of records into collections with the same name as the corresponding
%%      record type. If a record contains nested records with '_id' fields, the nested documents are 
%%      upsrted into their appropriate collections as well. A list of IDs of the inserted documents is
%%      returned.
%%
%% @spec insert_all(list(record())) -> list(bson:value())
%% @end
insert_all(Records) ->
	[insert(Record) || Record <- Records].
	
%% @doc Updates selected documents using a modifier. The modifier can be specified as a {key, value}
%%      tuple where the key is a modifier and the value is a mapped record (e.g {'$set', #foo{bar=3}}) 
%%      or as a mongo modifier tuple (e.g. {'$set', {bar, 3}).
%%
%% @spec modify(record(), record()|tuple()) -> ok
%% @end
modify(SelectorRecord, ModifierRecord) ->
	Collection = mongrel_mapper:get_type(SelectorRecord),
	Selector = mongrel_mapper:map_selector(SelectorRecord),
	Modifier = mongrel_mapper:map_modifier(ModifierRecord),
	mongo:modify(Collection, Selector, Modifier).

replace(SelectorRecord, NewRecord) ->
	{{Collection, NewDocument}, ChildDocuments} = mongrel_mapper:map(NewRecord),
	Selector = mongrel_mapper:map_selector(SelectorRecord),
	mongo:replace(Collection, Selector, NewDocument),
	[mongo:save(ChildCollection, ChildDocument) || {ChildCollection, ChildDocument} <- ChildDocuments].
	
repsert(RecordSelector, NewRecord) ->
	{{Collection, NewDocument}, ChildDocuments} = mongrel_mapper:map(NewRecord),
	Selector = mongrel_mapper:map_selector(RecordSelector),
	mongo:repsert(Collection, Selector, NewDocument),
	[mongo:save(ChildCollection, ChildDocument) || {ChildCollection, ChildDocument} <- ChildDocuments].
	
%% @doc Upserts a record into a collection with the same name as the record type. If the 
%%      record contains nested records with '_id' fields, the nested documents are upserted
%%      into their appropriate collections as well.
%%
%% @spec save(record()) -> ok
%% @end
save(Record) ->
	{{Collection, Document}, ChildDocuments} = mongrel_mapper:map(Record),
	[mongo:save(ChildCollection, ChildDocument) || {ChildCollection, ChildDocument} <- ChildDocuments],
	mongo:save(Collection, Document).
	

%% Server functions

%% @doc Initializes the server with a MongoDB connection.
%% @spec init(MongoDbConnection) -> {ok, State::tuple()}
%% @end
init([WriteMode, ReadMode, Connection, Database]) ->
	put(write_mode, WriteMode),
	put(read_mode, ReadMode),
	put(connection, Connection),
	put(database, Database),
    {ok, #state{}}.

%% @doc Responds synchronously to server calls.
%% @spec handle_call(Message::tuple(), From::pid(), State::tuple()) -> {stop, normal, Reply::any(), NewState::tuple()}
%% @end
handle_call({do, WriteMode, ReadMode, Connection, Database, Action}, _From, State) ->
    Reply = mongo:do(WriteMode, ReadMode, Connection, Database, Action),
    {stop, normal, Reply, State}.

%% @doc Responds asynchronously to messages.
%% @spec handle_cast(any(), tuple()) -> {no_reply, State}
%% @end
handle_cast(_Message, State) ->
	{noreply, State}.

%% @doc Responds to non-OTP messages.
%% @spec handle_info(any(), tuple()) -> {no_reply, State}
%% @end
handle_info(_Info, State) ->
	{noreply, State}.

%% @doc Handles the shutdown of the server.
%% @spec terminate(any(), any()) -> ok
%% @end
terminate(_Reason, _State) ->
	ok.

%% @doc Responds to code changes.
%% @spec code_change(any(), any(), any()) -> {ok, State}
%% @end
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.
