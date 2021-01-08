package userInput

trait Action

trait NonQuitAction extends Action {
  def run[A]: A
}

trait QuitAction extends Action
