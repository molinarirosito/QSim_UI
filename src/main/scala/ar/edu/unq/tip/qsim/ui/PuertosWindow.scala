package ar.edu.unq.tip.qsim.ui

import java.awt.Color

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.Map

import org.apache.commons.lang.StringUtils
import org.uqbar.arena.Application
import org.uqbar.arena.actions.MessageSend
import org.uqbar.arena.bindings.ObservableProperty
import org.uqbar.arena.layout.{ ColumnLayout, HorizontalLayout, VerticalLayout }
import org.uqbar.arena.widgets._
import org.uqbar.arena.widgets.tables.Column
import org.uqbar.arena.widgets.tables.Table
import org.uqbar.arena.windows.{ Dialog, Window, WindowOwner }
import org.uqbar.commons.utils.{ Observable, ReflectionUtils, When }
import com.uqbar.commons.collections.Transformer
import ar.edu.unq.tpi.qsim.model.State._
import ar.edu.unq.tpi.qsim.model._
import ar.edu.unq.tpi.qsim.utils._

class PuertosWindow(owner: WindowOwner, model: SimuladorAppmodel) extends Dialog[SimuladorAppmodel](owner, model) {

  override def createFormPanel(mainPanel: Panel) = {
    this.setTitle("Qsim - Puertos")
    // de esta forma se tiene que poner cuando generamos el .jar
    //this.setIconImage("icon.png")
    // de esta otra forma es para desarrollo
     this.setIconImage(getClass().getResource("/icon.png").getPath())
    var form = new Panel(mainPanel)
    form.setLayout(new VerticalLayout())
    crearPanelDePuertos(form)
    agregarBoton(form)
  }

  def agregarBoton(parent: Panel) {
    var form = new Panel(parent)
    form.setLayout(new HorizontalLayout())
    new Label(form).setWidth(273)

    val editable = new Button(form)
      .setCaption("Editable")
      .onClick(new MessageSend(model, "cambiarEdicion"))
      .setAsDefault
  }

  def crearPanelDePuertos(parent: Panel) {
    var panelForm = new Panel(parent)
    var contador = 0
    panelForm.setLayout(new ColumnLayout(4))
    model.sim.busIO.puertos.celdas.foreach(puerto ⇒ {
      val puertosPanel = new Panel(panelForm, puerto)
      puertosPanel.setLayout(new ColumnLayout(2))
      new Label(puertosPanel).setText(Util.toHex4(65520 + contador) + ":")
      contador = contador + 1
      val text = new TextBox(puertosPanel)
      text.bindEnabled(new ObservableProperty(this.getModelObject(), "enabled"))
      text.bindValueToProperty("value.hex")
      text.withFilter(new TextFilter() {
        def accept(event: TextInputEvent): Boolean = {
          event.getPotentialTextResult().matches("[A-F0-9]{0,4}")
        }
      })
      text.bindBackgroudToTransformer("state", new Transformer[Type, Color]() {
        def transform(element: Type) = element match {
          case NONE ⇒ Color.WHITE
          case PROGRAM ⇒ Color.LIGHT_GRAY
          case STORE ⇒ Color.BLUE
          case FECH_DECODE ⇒ Color.GREEN
          case EXECUTED ⇒ Color.CYAN
          case _ ⇒ null
        }
      })
    })
  }
}