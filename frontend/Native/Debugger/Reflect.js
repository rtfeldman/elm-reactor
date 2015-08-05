Elm.Native = Elm.Native || {};
Elm.Native.Debugger = Elm.Native.Debugger || {};
Elm.Native.Debugger.Reflect = Elm.Native.Debugger.Reflect || {};

Elm.Native.Debugger.Reflect = {};
Elm.Native.Debugger.Reflect.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debugger = localRuntime.Native.Debugger || {};
	localRuntime.Native.Debugger.Reflect = localRuntime.Native.Debugger.Reflect || {};
	if ('values' in localRuntime.Native.Debugger.Reflect)
	{
		return localRuntime.Native.Debugger.Reflect.values;
	}

	var VirtualDom = Elm.VirtualDom.make (localRuntime);
	var List = Elm.List.make (localRuntime);
	var NativeList = Elm.Native.List.make (localRuntime);
	var ElmArray = Elm.Array.make (localRuntime);
	var Dict = Elm.Dict.make (localRuntime);
	var Set = Elm.Set.make (localRuntime);
	var Utils = Elm.Native.Utils.make (localRuntime);

	function getHtml(mainVal)
	{
		// TODO: I hear this is bad (http://webreflection.blogspot.com/2013/03/5-reasons-you-should-avoid-proto.html)
		// but instanceof didn't work for some reason. find a workaround.
		if(mainVal.__proto__.type == 'VirtualNode' || mainVal.__proto__.type == 'VirtualText') {
			return mainVal;
		} else if(mainVal.element && mainVal.props) {
			return VirtualDom.fromElement(mainVal);
		} else {
			throw new Error('not Html or Element');
		}
	}

	function jsRepr(val)
	{
		return val;
	}

	function decode(v)
	{
		var type = typeof v;
		if (type === 'function') {
			var name = v.func ? v.func.name : v.name;
			return {ctor: 'Function', _0: name};
		} else if (type === 'boolean') {
			return {ctor: 'Boolean', _0: v};
		} else if (type === 'number') {
			return {ctor: 'Number', _0: v}
		} else if ((v instanceof String) && v.isChar) {
			return {ctor: 'Chr', _0: v};
		} else if (type === 'string') {
			return {ctor: 'Str', _0: v};
		} else if (type === 'object' && '_' in v && probablyPublic(v)) {
			var args = [];
			for (var k in v._) {
				// not sure what is in here
				for (var i = v._[k].length; i--; ) {
					args.push(Utils.Tuple2(k, decode(v._[k][i])));
				}
			}
			for (var k in v) {
				if (k === '_') continue;
				args.push(Utils.Tuple2(k, decode(v[k])));
			}
			return {ctor: 'Record', _0: NativeList.fromArray(args)};
		} else if (type === 'object' && 'ctor' in v) {
			if (v.ctor.substring(0,6) === '_Tuple') {
				var output = [];
				for (var k in v) {
					if (k === 'ctor') continue;
					output.push(decode(v[k]));
				}
				return {ctor: 'TupleV', _0: NativeList.fromArray(output)};
			} else if (v.ctor === '_Array') {
				var list = decode(ElmArray.toList(v));
				return {ctor: 'ArrayV', _0: list._0};
			} else if (v.ctor === '::') {
				var items = [];
				items.push(decode(v._0));
				v = v._1;
				while (v && v.ctor === '::') {
					items.push(decode(v._0));
					v = v._1;
				}
				return {ctor: 'ListV', _0: NativeList.fromArray(items)};
			} else if (v.ctor === '[]') {
				return {ctor: 'ListV', _0: NativeList.fromArray([])};
			} else if (v.ctor === 'RBNode' || v.ctor === 'RBEmpty') {
				var list = Dict.toList(v);
				if (list.ctor === '::' && list._0._1.ctor === '_Tuple0') {
					return {
						ctor: 'SetV',
						_0: A2(List.map, function(x){return decode(x._0)}, list)
					};
				}
				function decodeBoth(pair)
				{
					return Utils.Tuple2(decode(pair._0), decode(pair._1))
				}
				return {
					ctor: 'DictV',
					_0: A2(List.map, decodeBoth, list)
				};
			} else {
				var args = [];
				for (var i in v) {
					if (i === 'ctor') continue;
					args.push(decode(v[i]));
				}
				return {
					ctor: 'Constructor',
					_0: v.ctor,
					_1: NativeList.fromArray(args)
				};
			}
		}
		if (type === 'object' && 'notify' in v) {
			return {ctor: 'SignalV'}
		}
		return {ctor: 'NativeVal', _0: v};
	}

	// not exposed
	function addSlashes(str)
	{
		return str.replace(/\\/g, '\\\\')
				  .replace(/\n/g, '\\n')
				  .replace(/\t/g, '\\t')
				  .replace(/\r/g, '\\r')
				  .replace(/\v/g, '\\v')
				  .replace(/\0/g, '\\0')
				  .replace(/\'/g, "\\'")
				  .replace(/\"/g, '\\"');
	}

	// not exposed
	function probablyPublic(v)
	{
		var keys = Object.keys(v);
		var len = keys.length;
		if (len === 3
			&& 'props' in v
			&& 'element' in v) return false;
		if (len === 5
			&& 'horizontal' in v
			&& 'vertical' in v
			&& 'x' in v
			&& 'y' in v) return false;
		if (len === 7
			&& 'theta' in v
			&& 'scale' in v
			&& 'x' in v
			&& 'y' in v
			&& 'alpha' in v
			&& 'form' in v) return false;
		return true;
	}


	return localRuntime.Native.Debugger.Reflect.values = {
		getHtml: getHtml,
		decode: decode,
		jsRepr: jsRepr
	};
};
