package horstmann
package section6

class Account2 /*private*/ (_id: Int, initialBalance: Double) {

  val id = _id
  private[this] var _deposit = initialBalance

  def deposit(sum: Double) = _deposit += sum

  def deposit = _deposit

  def +(sum: Double): Account2 = {
    deposit(sum);
    this
  }

}

object Account2 {

  private var id = 0;

  def genId(): Int = {
    id += 1;
    id
  }

  def apply(initialBalance: Double): Account2 = new Account2(genId(), initialBalance)

}