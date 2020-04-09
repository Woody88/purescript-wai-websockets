"use strict";

exports.onupgrade = svr => cb => () => {
    svr.on('upgrade', (request, socket, head) => {
        cb(request)(socket)(head)()
    })   
}