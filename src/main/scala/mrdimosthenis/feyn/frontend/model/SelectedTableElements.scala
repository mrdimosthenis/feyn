package mrdimosthenis.feyn.frontend.model

case class SelectedTableElements(selectedQubitIndex: Option[Int], selectedSwitchIndex: Option[Int])

object SelectedTableElements {

  def init(): SelectedTableElements =
    SelectedTableElements(None, None)

}
