const { RPCConnection } = require('./stdio-jrpc')
const jieba = require('nodejieba')

// Entry
const main = () => {

  let conn = RPCConnection.open()

  conn.handle('hello', (_params) => {
    jieba.load()
    conn.sendResult(true)
  })

  conn.handle('split', (params) => {
    // Using tag method will get the best result,
    // even better than cut and cutHMM.
    let result = jieba.tag(params).map((it, _idx, _arr) => it.word)
    conn.sendResult(result)
  })

  conn.handle('loadDict', (params) => {
    let success = 0
    let failed = 0
    for (dict of params) {
      try {
        jieba.load({
          userDict: dict
        })
        success += 1
      } catch (e) {
        console.error(e)
        failed += 1
      }
    }
    conn.sendResult(`[JIEBA] Try to load ${params.length} Dict(s), ${success} successed, ${failed} failed.`)
  })
}

main()
