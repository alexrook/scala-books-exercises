package horstmann

class Account {

  val id = Account.genId()
  private[this] var _deposit = 0d

  def deposit(sum: Double) = _deposit += sum

  def deposit = _deposit

  def +(sum: Double): Account = {
    deposit(sum);
    this
  }

}

object Account {

  private var id = 0;

  def genId(): Int = {
    id += 1;
    id
  }

  def apply(): Account = new Account
}