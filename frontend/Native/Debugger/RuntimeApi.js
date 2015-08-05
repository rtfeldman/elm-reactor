Elm.Native = Elm.Native || {};
Elm.Native.Debugger = Elm.Native.Debugger || {};
Elm.Native.Debugger.RuntimeApi = Elm.Native.Debugger.RuntimeApi || {};

Elm.Native.Debugger.RuntimeApi = {};
Elm.Native.Debugger.RuntimeApi.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debugger = localRuntime.Native.Debugger || {};
	localRuntime.Native.Debugger.RuntimeApi = localRuntime.Native.Debugger.RuntimeApi || {};
	if ('values' in localRuntime.Native.Debugger.RuntimeApi)
	{
		return localRuntime.Native.Debugger.RuntimeApi.values;
	}

	var Signal = Elm.Native.Signal.make (localRuntime);
	var Task = Elm.Native.Task.make (localRuntime);
	var Utils = Elm.Native.Utils.make (localRuntime);
	var List = Elm.Native.List.make (localRuntime);
	var Dict = Elm.Dict.make (localRuntime);

	function sgShape(session) {
		return session.shape;
	}

	function getModule(session) {
		return session.module;
	}

	function getAddress(session) {
		return session.notificationAddress;
	}

	function getSubscriptions(session) {
		return Task.asyncFunction(function(callback) {
			callback(Task.succeed(List.fromArray(session.subscribedNodeIds)));
		});
	}

	function numFrames(session) {
		return session.events.length + 1;
	}

	// QUERIES

	function getNodeState(session, frameInterval, nodeIds)
	{
		return Task.asyncFunction(function(callback) {
			assertNotDisposed(session, callback, function() {
				assertPaused(session, callback, function() {
					nodeIds = List.toArray(nodeIds);

					jumpTo(session, frameInterval.start);

					// go through the target range
					var valueLogs = {};
					nodeIds.forEach(function(nodeId) {
						valueLogs[nodeId] = [];
					});
					for(var idx = frameInterval.start; idx <= frameInterval.end; idx++)
					{
						// get values
						nodeIds.forEach(function(nodeId) {
							valueLogs[nodeId].push(Utils.Tuple2(idx, session.sgNodes[nodeId].value));
						});
						// push event
						if(idx < frameInterval.end)
						{
							var event = session.events[idx];
							session.originalNotify(event.nodeId, event.value);
						}
					}

					var logs = nodeIds.map(function(nodeId) {
						return Utils.Tuple2(nodeId, List.fromArray(valueLogs[nodeId]));
					});

					callback(Task.succeed(List.fromArray(logs)));
				});
			});
		});
	}

	function getInputHistory(session)
	{
		return Task.asyncFunction(function(callback) {
			callback(Task.succeed(session.events));
		});
	}

	var emptyInputHistory = [];

	function splitInputHistory(frameIdx, history)
	{
		return Utils.Tuple2(history.slice(0, frameIdx), history.slice(frameIdx));
	}

	function evalModule(compiledModule) {
		window.eval(compiledModule.code);
		var elmModule = Elm;
		var names = compiledModule.name.split('.');
		for (var i = 0; i < names.length; ++i)
		{
			elmModule = elmModule[names[i]];
		}
		return elmModule;
	}

	// COMMANDS

	function initializeFullscreen(module, inputHistory, notificationAddress, initialNodesFun)
	{
		return Task.asyncFunction(function(callback) {
			var debugeeLocalRuntime;
			var moduleBeingDebugged = Elm.fullscreen({
				make: function(runtime) {
					debugeeLocalRuntime = runtime;
					return module.make(runtime);
				}
			}, {}, false);

			var sgNodes = flattenSignalGraph(debugeeLocalRuntime);
			var sgShape = getSgShape(sgNodes);
			var session = {
				module: module,
				runningModule: moduleBeingDebugged,
				runtime: debugeeLocalRuntime,
				originalNotify: debugeeLocalRuntime.notify,
				sgNodes: sgNodes,
				delay: 0, // TODO: think delay stuff through!
				// TODO: delay, totalTimeLost, asyncCallbacks
				asyncCallbacks: [],
				events: inputHistory,
				snapshots: [takeSnapshot(sgNodes)],
				shape: sgShape,
				notificationAddress: notificationAddress,
				disposed: false,
				playing: true,
				subscribedNodeIds: List.toArray(initialNodesFun(sgShape)),
				flaggedExprValues: []
			};

			function getSgShape(nodes) {
				var mainId;
				var nodeTuples = Object.keys(nodes).map(function(nodeId) {
					var node = nodes[nodeId];
					var nodeType;
					if(node.name == 'input-mailbox') {
						nodeType = {ctor: 'Mailbox'};
					} else if(node.name.indexOf('input') == 0) {
						nodeType = {ctor: 'CoreLibInput'};
					} else if(node.isOutput && node.isOutput) {
						if(node.name == 'output-main') {
							nodeType = {ctor:'Main'};
							mainId = node.id;
						} else {
							nodeType = {ctor:'OutputPort'}
						}
					} else {
						nodeType = {ctor: 'InternalNode'};
					}
					var info = {
						_: {},
						name: node.name,
						nodeType: nodeType,
						kids: List.fromArray(
							node.kids ? node.kids.map(function(kid) {return kid.id}) : []
						)
					};
					return Utils.Tuple2(node.id, info);
				});
				return {
					_: {},
					nodes: Dict.fromList(List.fromArray(nodeTuples)),
					mainId: mainId
				}
			}

			// set up event recording
			debugeeLocalRuntime.notify = function(id, value) {
				if (!session.playing)
				{
					return false;
				}

				session.flaggedExprValues = [];

				var changed = session.originalNotify(id, value);

				// Record the event
				var event = {
					_: {},
					value: value,
					nodeId: id,
					time: session.runtime.timer.now()
				}
				session.events.push(event);
				// take snapshot if necessary
				if(session.events.length % EVENTS_PER_SAVE == 0)
				{
					session.snapshots.push(takeSnapshot(session.sgNodes));
				}
				
				var subscribedNodeValues = session.subscribedNodeIds.map(function(nodeId) {
					var node = session.sgNodes[nodeId];
					return Utils.Tuple2(nodeId, node.value);
				});
				// send notification
				var notification = {
					_: {},
					event: event,
					flaggedExprValues: List.fromArray(session.flaggedExprValues),
					subscribedNodeValues: List.fromArray(subscribedNodeValues)
				}
				Task.perform(notificationAddress._0(notification));

				// TODO: add traces

				return changed;
			};

			debugeeLocalRuntime.setTimeout = function(thunk, delay) {
				if (!session.playing)
				{
					return 0;
				}

				var callback = {
					thunk: thunk,
					id: 0,
					executed: false
				};

				callback.id = setTimeout(function() {
					callback.executed = true;
					thunk();
				}, delay);

				// TODO: this isn't fully hooked up yet
				session.asyncCallbacks.push(callback);
				return callback.id;
			};

			debugeeLocalRuntime.timer.now = function() {
				// TODO: not sure how to get time of last event
				// if (debugState.paused || debugState.swapInProgress)
				// {
				// 	var event = debugState.events[debugState.index];
				// 	return event.time;
				// }
				return Date.now() - session.delay;
			};
			debugeeLocalRuntime.debug = {
				log: function(tag, value) {
					if (!session.playing)
					{
						return;
					}
					session.flaggedExprValues.push(Utils.Tuple2(tag, value));
				},
				trace: function(tag, form) {
					// TODO: ...
					return replace([['trace', tag]], form);
				}
			};

			// get values of initial subscription

			var initNodeVals = session.subscribedNodeIds.map(function(nodeId) {
				return Utils.Tuple2(nodeId, session.sgNodes[nodeId].value);
			});
			
			var result = Utils.Tuple2(session, List.fromArray(initNodeVals));

			callback(Task.succeed(result));
		});
	}

	function dispose(session)
	{
		return Task.asyncFunction(function(callback) {
			session.disposed = true;
			session.runningModule.dispose();
			callback(Task.succeed(Utils.Tuple0));
		});
	}

	function setPlaying(session, playing)
	{
		return Task.asyncFunction(function(callback) {
			assertNotDisposed(session, callback, function() {
				if(session.playing) {
					if(!playing) {
						// PAUSE
						// TODO asyncCallback stuff for timers
						session.playing = playing;
						callback(Task.succeed(Utils.Tuple0));
					} else {
						callback(Task.fail(Utils.Tuple0));
					}
				} else {
					if(playing) {
						// PLAY
						session.playing = playing;
						callback(Task.succeed(Utils.Tuple0));
					} else {
						callback(Task.fail(Utils.Tuple0));
					}
				}
				callback(Task.succeed(Utils.Tuple0));
			});
		});
	}

	function setSubscribedToNode(session, nodeId, subscribed)
	{
		return Task.asyncFunction(function(callback) {
			assertNotDisposed(session, callback, function() {
				var idx = session.subscribedNodeIds.indexOf(nodeId);
				var alreadySubscribed = idx != -1;
				if(subscribed) {
					if(alreadySubscribed) {
						callback(Task.fail(Utils.Tuple0));
					} else {
						session.subscribedNodeIds.push(nodeId);
						callback(Task.succeed(Utils.Tuple0));
					}
				} else {
					if(alreadySubscribed) {
						session.subscribedNodeIds.splice(idx, 1);
						callback(Task.succeed(Utils.Tuple0));
					} else {
						callback(Task.fail(Utils.Tuple0));
					}
				}
			});
		});
	}

	// not exposed
	function jumpTo(session, frameIdx)
	{
		// get to it
		var snapshotBeforeIdx = Math.floor(frameIdx / EVENTS_PER_SAVE);
		var snapshot = session.snapshots[snapshotBeforeIdx];
		for(var nodeId in snapshot) {
			session.sgNodes[nodeId].value = snapshot[nodeId];
		}
		var snapshotBeforeFrameIdx = snapshotBeforeIdx * EVENTS_PER_SAVE;
		for(var idx=snapshotBeforeFrameIdx; idx < frameIdx; idx++)
		{
			var event = session.events[idx];
			session.originalNotify(event.nodeId, event.value);
		}
	}

	// Bool -> a -> (Task -> ()) -> (() -> ()) -> ???
	function assert(bool, err, callback, thunk)
	{
		if(!bool) {
			callback(Task.fail(err));
		} else {
			thunk();
		}
	}

	function assertNotDisposed(session, callback, thunk)
	{
		assert(!session.disposed, {ctor: "IsDisposed"}, callback, thunk);
	}

	function assertPaused(session, callback, thunk)
	{
		assert(!session.disposed, {ctor: "IsPlaying"}, callback, thunk);
	}

	return localRuntime.Native.Debugger.RuntimeApi.values = {
		sgShape: sgShape,
		getModule: getModule,
		getAddress: getAddress,
		getSubscriptions: getSubscriptions,
		numFrames: numFrames,
		getNodeState: F3(getNodeState),
		getInputHistory: getInputHistory,
		splitInputHistory: F2(splitInputHistory),
		emptyInputHistory: emptyInputHistory,
		evalModule: evalModule,
		initializeFullscreen: F4(initializeFullscreen),
		dispose: dispose,
		setPlaying: F2(setPlaying),
		setSubscribedToNode: F3(setSubscribedToNode)
	};
};

// Utils

var EVENTS_PER_SAVE = 100;

// returns array of node references, indexed by node id (?)
function flattenSignalGraph(runtime) {
	var nodesById = {};

	function addAllToDict(node)
	{
		nodesById[node.id] = node;
		if(node.kids) {
			node.kids.forEach(addAllToDict);
		}
	}
	runtime.inputs.forEach(addAllToDict);

	return nodesById;
}


// returns snapshot
function takeSnapshot(signalGraphNodes)
{
	var nodeValues = {};

	Object.keys(signalGraphNodes).forEach(function(nodeId) {
		var node = signalGraphNodes[nodeId];
		nodeValues[nodeId] = node.value;
	});

	return nodeValues;
}
