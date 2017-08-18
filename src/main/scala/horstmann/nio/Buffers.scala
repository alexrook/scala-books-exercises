package horstmann.nio

import java.nio.ByteBuffer

object BufferUtils {

  def loopBuffers(model: ByteBuffer,
                  source: ByteBuffer,
                  pos: Int, limit: Int,
                  f: (Byte, Byte) => Boolean): Boolean = if (pos < limit) {
    val modelByte = model.get(pos)
    val srcByte = source.get(pos)
    f(modelByte, srcByte) && loopBuffers(model, source, pos + 1, limit, f)
  } else true //empty buffers also equals ?


  def loopBuffer(source: ByteBuffer, f: (Byte) => Boolean): Unit = if (source.hasRemaining
    && f(source.get())) loopBuffer(source, f)


  /**
    * Copies good data from the source buffer
    *
    * @param source
    * @return new buffer with capacity=source.limit and data from source buffer
    */
  def duplicateBuffer(source: ByteBuffer): ByteBuffer = {
    source.flip()
    val ret = ByteBuffer.allocateDirect(source.limit)
    loopBuffer(source, (byte) => {
      ret.put(byte)
      true
    })
    ret.flip()
    ret
  }

  /**
    * Compare two buffers by byte
    * no buffer state touching
    *
    * @param model      - reference buffer
    * @param source     - verifiable buffer
    * @param modelLimit - reference buffer limit
    * @param srcLimit   - verifiable buffer limit
    * @return - true If the model and source have
    *         the same limit and the same bytes in the same positions.
    */
  def compareBuffersAll(model: ByteBuffer, source: ByteBuffer, modelLimit: Int, srcLimit: Int): Boolean =
    (modelLimit == srcLimit) &&
      loopBuffers(model, source, 0, modelLimit, (a, b) => a == b)

  def compareBuffersAll(model: ByteBuffer, source: ByteBuffer): Boolean =
    compareBuffersAll(model, source, model.limit, source.limit)

  /**
    * Comparing two buffers by bytes that are available in the source
    * no buffer state touching
    *
    * @param model    - reference buffer
    * @param source   - verifiable buffer
    * @param srcLimit - limit for verifiable buffer
    * @return - If the source and model have
    *         the same bytes in the same positions.
    */
  def compareBuffersHead(model: ByteBuffer, source: ByteBuffer, srcLimit: Int): Boolean =
    (srcLimit <= model.limit) &&
      loopBuffers(source, model, 0, srcLimit, (a, b) => a == b)

}