import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.{ActorMaterializer, Materializer}
import korolev._
import korolev.akkahttp._
import korolev.execution._
import korolev.server._
import korolev.state.javaSerialization._

import scala.concurrent.Future
import MyState.globalContext._
import symbolDsl._

object tabs extends App {

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val materializer: Materializer = ActorMaterializer()

  val applicationContext = Context[Future, MyState, Any]
  val inputId = elementId(Option("input"))
  val loginInputId = elementId(Option("login"))

  val generateEmptyState = () => {
    new MyState(
      name = "",
      messages = List[MyMessage]()
    )
  }

  val renderLogin = (state: MyState) =>
    'div (
      'form (
        'input (
          loginInputId,
          'placeholder /= "Login"
        ),
        'button (
          'type /= "submit",
          "Login"
        ),
        event('submit) { access =>
          access.property(loginInputId, 'value) flatMap( { value =>
            access.transition {
              _.copy(name = value)
            }
          })
        }
      )
    )

  val renderChat = (state: MyState) =>
    Seq(
      'p (
        "Your name is " + state.name
      ),
      'div (
        'index /= "messagesContainer",
        state.messages map (msg => {
          'div (
            'class /= "messageText",
            'style /= "padding: 10px",
            msg.author + ": " + msg.text
          )
        })
      ),
      'div (
        'index /= "inputWrapper",
        'form (
          'textarea (
            inputId,
            'style /= "display: block;"
          ),
          'button (
            'type /= "submit",
            "Send"
          ),
          event('submit) { access =>
            val prop = access.property(inputId)
            prop.get('value) flatMap { value =>
              prop.set('value, "") flatMap { _ =>
                access.transition { transition => {
                    if (value.length > 0) {
                      val msgs = state.messages :+ new MyMessage(state.name, value)
                      transition.copy(messages = msgs)
                    } else {
                      transition
                    }
                  }
                }
              }
            }
          }
        )
      )
    )

  private val config = KorolevServiceConfig[Future, MyState, Any](
    stateStorage = StateStorage.default(generateEmptyState()),
    router = emptyRouter,
    render = {
      case state: MyState => {
        'body (
          if (state.name == "") {
            renderLogin(state)
          } else {
            renderChat(state)
          }
        )
      }
    }
  )

  val head = Seq(
    'meta('charset /= "utf-8"),
    'meta('httpEquiv /= "X-UA-Compatible", 'content /= "IE=edge"),
    'meta('name /= "viewport", 'content /= "width=device-width, initial-scale=1"),
  )

  private val route = akkaHttpService(config).apply(AkkaHttpServerConfig())

  Http().bindAndHandle(route, "0.0.0.0", 8080)
}
