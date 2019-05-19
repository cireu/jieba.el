const jieba = require('nodejieba')

const rpcReturn = (id, retval) => JSON.stringify({
  jsonrpc: '2.0',
  id: id,
  result: retval,
})

const rpcError = (id, errCode, msg) => JSON.stringify({
  jsonrpc: '2.0',
  id: id,
  error: { code: errCode, message: msg },
})

const rpcInvalidRequest = () => rpcError(null, -32600, 'Invalid Request.')

const handleCut = (s) => {
  try {
    s = JSON.parse(s)
    let { jsonrpc, id, method, params } = s
    if (jsonrpc !== '2.0' || method !== 'split') {
      process.stdout.write(rpcInvalidRequest())
    } else {
      let words = jieba.cut(params)
      process.stdout.write(rpcReturn(id, words))
    }
  } catch (e) {
    console.error(e)
    process.stdout.write(rpcInvalidRequest())
  }
}

const main = () => {
  jieba.load()
  process.stdin.setEncoding('utf-8')
  process.stdin.on('readable', () => {
    let data = ''
    let chunck
    while (true) {
      chunck = process.stdin.read()
      if (chunck === null) {
        break
      } else {
        data += chunck
      }
    }
    handleCut(data)
  })
}

main()
