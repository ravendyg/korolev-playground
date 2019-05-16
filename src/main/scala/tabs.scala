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
import levsha.Document.Empty
import symbolDsl._

object tabs extends App {

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val materializer: Materializer = ActorMaterializer()

  val applicationContext = Context[Future, MyState, Any]
  val inputId = elementId(Option("input"))
  val loginInputId = elementId(Option("login"))

  val generateEmptyState = () => {
    new MyState(
      tab = 0,
      xmls = List(
        new XmlDoc("item 1", true, List(
          new XmlDoc("item 1_1", true, List()),
          new XmlDoc("item 1_2", true, List(
            new XmlDoc("item 1_2_1", true, List()),
          )),
        )),
        new XmlDoc("item 2", true, List()),
        new XmlDoc("item 3", true, List(
          new XmlDoc("item 3_1", true, List()),
        )),
      ),
    )
  }

  object Columns extends Enumeration {
    val CardNumber, FirstName, FathersName, LastName, SerPasp, NumPasp, RegNumber, BirthDate,
    CbCode, Login, id, Password, ContrStatus, AddInfoCn, TB_Code, TB_OSB,
    TB_Office, Phone = Value
  }

  def ModalCard(title: String,
                display: String = "flex",
                showCloseButton: Boolean = false,
                style: Attr = Empty)(fields: Node*)(buttons: Node*) = {
    'div (
      'class /= "modal",
      'style /= s"display: $display;", //
      'div ('class /= "modal-background"),
      'div (
        'class /= "modal-card",
        style, //
        'header ('class /= "modal-card-head",
          'p ('class /= "modal-card-title", title),
          if (showCloseButton) 'a ( 'href /= "",'class /= "delete") else Empty
        ),
        'section ('class /= "modal-card-body", fields),
        'footer ('class /= "modal-card-foot",
          //'a ( 'class /= "button is-success", disabledFlag, "Login")
          buttons)
      )
    )
  }

  def TextFieldWithLabel(
                          label: String,
                          tpe: String = "text",
                          value: String = null,
                          floating: Boolean = false)(elementId: Node)(attr: Attr*) = {
    val valueOpt = Option(value)
    'div ('class /= "field",
      'label ('class /= "label", label),
      'p ('class /= "control",
        'input ('type /= tpe,
          'class /= "input",
          valueOpt.map(s => 'value /= s),
          elementId,
          attr)))
  }

  def renderTabHeader(state: MyState) =
    'div (
      'class /= "tabs",
      'ul (
        'li (
          'class /= (if (state.tab == 0) "is-active" else ""),
          'a (
            "Tab 1",
            event('click) {
              access => {
                access.transition {
                  transition => transition.copy(tab = 0)
                }
              }
            }
          )
        ),
        'li (
          'class /= (if (state.tab == 1) "is-active" else ""),
          'a (
            "Tab 2",
            event('click) {
              access => {
                access.transition {
                  transition => transition.copy(tab = 1)
                }
              }
            }
          )
        ),
        'li (
          'class /= (if (state.tab == 2) "is-active" else ""),
          'a (
            "Tab 3",
            event('click) {
              access => {
                access.transition {
                  transition => transition.copy(tab = 2)
                }
              }
            }
          )
        )
      )
    )


  def renderTab1() =
    'div (
      'div (
        'class /= "columns",
        'div (
          'class /= "column",
          TextFieldWithLabel("Имя *", floating = true, tpe = "userID")(Empty)(
            'name /= Columns.FirstName.toString,
            'pattern /= "\\[a-z0-9._%]{1,20}", //a-z только строчные буквы. Возможно нужно разрешить прописные.
            'required /= "",
            'maxlength /= "20")
        ),
        'div (
          'class /= "column",
          TextFieldWithLabel("Фамилия *", floating = true, tpe = "userID")(Empty)(
            'name /= Columns.LastName.toString,
            'pattern /= "\\[a-z0-9._%]{1,20}",
            'required /= "",
            'maxlength /= "20")
        ),
        'div (
          'class /= "column",
          TextFieldWithLabel("Отчество", floating = true, tpe = "userID")(Empty)(
            'pattern /= "\\[a-z0-9._%]{1,20}",
            'name /= Columns.FathersName.toString,
            'maxlength /= "20")
        )
      ),
      'div (
        'class /= "columns",
        'div (
          'class /= "column",
          TextFieldWithLabel("Дата рождения *", floating = true, tpe = "date", value = "1972-01-30")(Empty)(
            'name /= Columns.BirthDate.toString,
            'required /= "")
        ),
        'div (
          'class /= "column",
          TextFieldWithLabel("Серия паспорта", floating = true, tpe = "text")(Empty)(
            'name /= Columns.SerPasp.toString,
            'pattern /= "\\d{1,4}",
            'placeholder /= "xxxx",
            'title /= "Поле должно иметь числовое значение",
            'maxlength /= "4")
        ),
        'div (
          'class /= "column",
          TextFieldWithLabel("Номер паспорта *", floating = true, tpe = "text")(Empty)(
            'name /= Columns.NumPasp.toString,
            'pattern /= "\\d{1,6}",
            'placeholder /= "xxxxxx",
            'title /= "Поле должно иметь числовое значение",
            'required /= "",
            'maxlength /= "6")
        ),
        'div ('class /= "column",
          TextFieldWithLabel("Телефон *", floating = true, tpe = "tel")(Empty)(
            'name /= Columns.Phone.toString,
            'required /= "",
            'pattern /= "7\\d{10}",
            'placeholder /= "7xxxxxxxxxx",
            'maxlength /= "11"
          )
        )
      ),
      'div (
        'class /= "columns",
        'div (
          'class /= "column",
          TextFieldWithLabel("Логин *", floating = true, tpe = "text")(Empty)(
            'name /= Columns.Login.toString,
            'pattern /= "\\d{10}",
            'placeholder /= "1234567890",
            'title /= "Поле должно состоять из 10-ти цифр.",
            'required /= "",
            'maxlength /= "10",
            'style /= "width: 23%;")
        )
      ),
      'div (
        'class /= "columns",
        'div (
          'class /= "column",
          TextFieldWithLabel("Номер карточки *", floating = true, tpe = "text")(Empty)(
            'name /= Columns.CardNumber.toString,
            'pattern /= "\\d{16}",
            'placeholder /= "xxxxxxxxxxxxxxxx",
            'title /= "Поле должно иметь числовое значение",
            'required /= "",
            'maxlength /= "16",
            'value /= "1234 5678 9012 3456"
          )
        ),
        'div (
          'class /= "column",
          TextFieldWithLabel("Тербанк *", floating = true, tpe = "text")(Empty)(
            'name /= Columns.TB_Code.toString,
            'pattern /= "\\d{1,4}",
            'placeholder /= "xxxx",
            'title /= "Поле должно иметь числовое значение, например 99",
            'required /= "",
            'maxlength /= "4")
        ),
        'div (
          'class /= "column",
          TextFieldWithLabel("ОСБ *", floating = true, tpe = "text")(Empty)(
            'name /= Columns.TB_OSB.toString,
            'pattern /= "\\d{1,4}",
            'placeholder /= "xxxx",
            'title /= "Поле должно иметь числовое значение, например 9038",
            'required /= "",
            'maxlength /= "4")
        ),
        'div (
          'class /= "column",
          TextFieldWithLabel("Код офиса *", floating = true, tpe = "text")(Empty)(
            'name /= Columns.TB_Office.toString,
            'pattern /= "\\d{1,7}",
            'placeholder /= "xxxxxxx",
            'title /= "Поле должно иметь числовое значение, например 16",
            'required /= "",
            'maxlength /= "7")
        )
      )
    )

  def renderTab2(nodes: List[XmlDoc]) =
    'div (
      nodes.map(renderXmlNode)
    )

  def renderTab3() =
    'div (
      "Tab 3"
    )

  // better to use some library to handle trees
  // this implementation would recreate every element (except those below the toggled one)
  // but as a quick and dirty hack it would work
  def toggleNodeVisibility(node: XmlDoc, doc: XmlDoc): XmlDoc =
    if (node.equals(doc))
        new XmlDoc(doc.text, !doc.expanded, doc.children)
    else
      new XmlDoc(doc.text, doc.expanded, doc.children.map(xml => toggleNodeVisibility(node, xml)))

  def renderArrow(node: XmlDoc, klass: String) =
    'span(
      'class /= "icon pointer",
      'i(
        'class /= klass
      ),
      event('click) { access =>
        access.transition {
          case state => state.copy(xmls = state.xmls.map(doc => toggleNodeVisibility(node, doc)))
        }
      }
    )

  def renderRigthArrow(node: XmlDoc) = renderArrow(node, "fa fa-arrow-right")

  def renderDownArrow(node: XmlDoc) = renderArrow(node, "fa fa-arrow-down")

  def renderXmlNode(doc: XmlDoc) : levsha.Document.Node[Context.Effect[Future, MyState, Any]] =
    'div(
      // icon
      if (doc.expanded) renderDownArrow(doc)
        else renderRigthArrow(doc),
      // content
      doc.text,
      // children
      if (doc.expanded) 'div(
          'class /= "xml-children-container",
          doc.children.map(renderXmlNode),
        ) else "",
    )

  def showFormNewClient(state: MyState) =
    'form (
      ModalCard("Ввод нового клиента", "flex", true, 'style /= "width: 840px;")(
        renderTabHeader(state),
        state.tab match {
          case 0 => renderTab1()

          case 1 => renderTab2(state.xmls)

          case 2 => renderTab3()
        }
      )(
        'div (
          'class /= "field is-grouped",
          'div (
            'class /= "field is-grouped",
            'p ('class /= "control",
              'button ('type /= "submit", 'class /= "button is-success", "Создать")),
            'p ('class /= "control",
              'a ('href /= "", 'class /= "button is-danger", "Отмена"))
          )
        )
      )
    )

  val head =
    Seq(
      'meta('charset /= "utf-8"),
      'meta('httpEquiv /= "X-UA-Compatible", 'content /= "IE=edge"),
      'meta('name /= "viewport", 'content /= "width=device-width, initial-scale=1"),
      'link('rel /= "stylesheet", 'href /= "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"),
      'link('rel /= "stylesheet", 'href /= "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.2/css/bulma.css"),
      // in hardcode we trust!
      'style(
        'type /= "text/css",
        """
          .xml-children-container {
            padding-left: 10px;
          }
          .pointer {
            cursor: pointer;
          }
        """
      )
    )

  private val config = KorolevServiceConfig[Future, MyState, Any](
    stateStorage = StateStorage.default(generateEmptyState()),
    router = emptyRouter,
    render = {
      case state: MyState =>
          'body (
            showFormNewClient(state)
          )
    },
    head = {
      head
    }
  )

  private val route = akkaHttpService(config).apply(AkkaHttpServerConfig())

  Http().bindAndHandle(route, "0.0.0.0", 8080)
}
