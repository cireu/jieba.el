// Helper functions
const buildRPCError = (id, errObj) => {
  let obj = {
    jsonrpc: '2.0',
    id: id,
    error: errObj
  }
 return JSON.stringify(obj)
}

const buildRPCResult = (id, result) => {
  return JSON.stringify({
    jsonrpc: '2.0',
    id: id,
    result: result,
  })
}

const RPCErrors = {
  ParseError (data = undefined) {
    return { code: -32700, data: data, message: 'Error during parsing.' }
  },
  InvalidRequest (data = undefined) {
    return { code: -32600, data: data, message: 'Invalid Request.' }
  },
  MethodNotFound (data = undefined) {
    return { code: -32601, data: data, message: 'Method Not Found.' }
  },
  InvalidParams (data = undefined) {
    return { code: -32602, data: data, message: 'Invalid Params.' }
  },
  InternalError (data = undefined) {
    return { code: -32603, data: data, message: 'Internal Error.' }
  },
}

class RPCConnection {

  _sendRaw (rawMsg) {
    process.stdout.write(rawMsg)
  }

  _receiveRaw (rawMsg) {
    let json
    try {
      json = JSON.parse(rawMsg)
    } catch (e) {
      console.error(e)
      this.sendError(RPCErrors.ParseError())
    }
    if (json != null) {
      let { jsonrpc, id, method, params } = json
      this._currentHandlingId = id
      if (jsonrpc !== '2.0') {
        this.sendError(RPCErrors.InvalidRequest())
      } else if (!this._methodTable.has(method)) {
        this.sendError(RPCErrors.MethodNotFound())
      } else {
        let m = this._methodTable.get(method)
        m(params)
      }
    }
    // Reset id
    this._currentHandlingId = null
  }

  // Export functions
  sendResult (result) {
    this._sendRaw(buildRPCResult(this._currentHandlingId, result))
  }

  sendError (errObj) {
    this._sendRaw(buildRPCError(this._currentHandlingId, errObj))
  }

  handle (methodName, callback) {
    this._methodTable.set(methodName, callback)
  }

  constructor () {
    this._methodTable = new Map()
    this._currentHandlingId = null

    process.stdin.setEncoding('utf-8')
    process.stdin.on('readable', () => {
      let data = ''
      let chunk
      while (true) {
        chunk = process.stdin.read()
        if (chunk === null) {
          break
        } else {
          data += chunk
        }
      }
      this._receiveRaw(data)
    })
  }

  static open () {
    if (RPCConnection._singleton == null) {
      RPCConnection._singleton = new RPCConnection()
    }
    return RPCConnection._singleton
  }
}

// Export
module.exports.RPCConnection = RPCConnection

module.exports.RPCErrors = RPCErrors
