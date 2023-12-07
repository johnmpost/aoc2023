"use strict";

export const matchAll = (regex) => (str) => {
    const re = new RegExp(regex, 'g');
    const matches = [...str.matchAll(re)];
    return matches.map(x => ({match: x[0], index: x.index}))
    // todo: also return the matched group as a Maybe
}
