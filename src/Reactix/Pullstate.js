let pullstate = require("pullstate");

exports._store = (data) => new pullstate.Store(data);

exports._useStoreState = (store, fun) => pullState.useStoreState(store, fun);
exports._useStoreStateN = (store, fun, memo) => pullState.useStoreState(store, fun, memo);

exports._subscribe = (store, watch, listen) => store.suscribe(watch, listen);

exports._update = (store, funs) => store.update(fun);

exports._reaction = (store, fun) => store.createReaction(fun);
