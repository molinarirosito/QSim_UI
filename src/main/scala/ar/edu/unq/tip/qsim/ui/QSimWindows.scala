package ar.edu.unq.tip.qsim.ui


/**
* Copyright 2014 Tatiana Molinari.
* Copyright 2014 Susana Rosito
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*
*/


import java.awt.Color
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.Map
import org.apache.commons.lang.StringUtils
import org.uqbar.arena.Application
import org.uqbar.arena.actions.MessageSend
import org.uqbar.arena.bindings.ObservableProperty
import org.uqbar.arena.layout.ColumnLayout
import org.uqbar.arena.layout.HorizontalLayout
import org.uqbar.arena.layout.VerticalLayout
import org.uqbar.arena.widgets.Button
import org.uqbar.arena.widgets.GroupPanel
import org.uqbar.arena.widgets.Label
import org.uqbar.arena.widgets.Link
import org.uqbar.arena.widgets.Panel
import org.uqbar.arena.widgets.TextBox
import org.uqbar.arena.widgets.TextFilter
import org.uqbar.arena.widgets.TextInputEvent
import org.uqbar.arena.widgets.tables.Column
import org.uqbar.arena.widgets.tables.Table
import org.uqbar.arena.windows.Dialog
import org.uqbar.arena.windows.Window
import org.uqbar.arena.windows.WindowOwner
import org.uqbar.commons.model.UserException
import org.uqbar.commons.utils.Observable
import org.uqbar.commons.utils.ReflectionUtils
import org.uqbar.commons.utils.When
import com.uqbar.commons.collections.Transformer
import ar.edu.unq.tpi.qsim.model.Celda
import ar.edu.unq.tpi.qsim.model.Programa
import ar.edu.unq.tpi.qsim.model.Simulador
import ar.edu.unq.tpi.qsim.model.State.EXECUTED
import ar.edu.unq.tpi.qsim.model.State.FECH_DECODE
import ar.edu.unq.tpi.qsim.model.State.NONE
import ar.edu.unq.tpi.qsim.model.State.PROGRAM
import ar.edu.unq.tpi.qsim.model.State.STORE
import ar.edu.unq.tpi.qsim.model.State.Type
import ar.edu.unq.tpi.qsim.model.W16
import ar.edu.unq.tpi.qsim.utils.Util
import javax.swing.GroupLayout
import ar.edu.unq.tpi.qsim.model.Memoria

@Observable
class Fila16Celdas(aName: String) extends Fila4Celdas(aName) {
  var celda4, celda5, celda6, celda7, celda8, celda9, celda10, celda11, celda12, celda13, celda14, celda15: Celda = _
}

@Observable
case class Fila4Celdas(name: String) {
  var celda0, celda1, celda2, celda3: Celda = _

  def apply(indice: Int, celda: Celda) {
    val field = ReflectionUtils.getField(this.getClass(), s"celda$indice")
    field.setAccessible(true)
    field.set(this, celda)
    When(celda) change "value" and "state" fireChange s"celda$indice" of this
  }
}

case class ModificarValorException(smth: String) extends UserException(smth)

@Observable
class SimuladorAppmodel(programa: Programa, pc: String = "0000") {
  var never_enabled = false
  var enabled = false
  var next = "0000"
  var prev = "0000"

  var sim = Simulador()
  sim.inicializarSim()
  var celdas: java.util.List[Fila16Celdas] = _
  var desde = new W16("0000")
  var hasta = new W16(Util.toHex4((16 * 32)))
  var nextVisible = true
  var prevVisible = true

  crearFila16Celdas()
  sim.cargarProgramaYRegistros(programa, pc, Map[String, W16]())

  def paginaInicial() {
    val diff = hasta - desde
    desde = new W16("0000")
    hasta = desde + diff
    prevVisible = false
    nextVisible = true
    crearFila16Celdas()
  }

  def paginaFinal() {
    val diff = hasta - desde
    hasta = new W16("FFF0")
    desde = hasta - diff
    prevVisible = true
    nextVisible = false
    crearFila16Celdas()
  }

  def paginaSiguiente() {
    var diff = hasta - desde
    if (hasta + diff > new W16("FFE0")) {
      diff = new W16("FFF0") - hasta
      nextVisible = false
    }
    prevVisible = true
    hasta += diff
    desde += diff
    crearFila16Celdas()
  }

  def paginaAnterior() {
    var diff = hasta - desde
    if (desde - diff < new W16("0000")) {
      diff = diff - desde
      prevVisible = false
    }
    nextVisible = true
    hasta -= diff
    desde -= diff
    crearFila16Celdas()
  }

  def crearFila16Celdas() {
    validarDesdeYHasta(desde, hasta)
    val list = new java.util.ArrayList[Fila16Celdas]()
    var contador = desde.value
    var memoria = sim.busIO.memoria
    var name = new W16(desde.hex)
    prev = <a> { name.hex }</a>.toString
    var row = 0
    var fila: Fila16Celdas = new Fila16Celdas(name.toString)
    
    do {
      if (row >= 16) {
        row = 0
        list.append(fila)
        name = name.+(new W16("0010"))
        fila = new Fila16Celdas(name.toString)
      }
      fila(row, memoria.celda(contador))
      contador = contador + 1
      row = row + 1
    } while (contador < hasta.value)
    list.append(fila)
    name = name
    next = <a>  { name.hex }  </a>.toString
    celdas = list
  }
  
  
  def validarDesdeYHasta(desde: W16, hasta: W16) {
    if (desde > hasta) {
      throw new UserException("Desde no puede ser mayor que hasta.")
    }

    if ((hasta - desde).value > 200 * 16) {
      throw new UserException(s"La diferencia maxima entre desde y hasta es ${200 * 16}.")
    }

    if (hasta > new W16("FFF0")) {
      throw new UserException(s"Hasta no puede superar el maximo a FFF0 que es el maximo de celdas de la memoria.")
    }

    if (desde < new W16("0000")) {
      throw new UserException(s"Desde no puede ser menor a 0000.")
    }
  }

  def vaciarRegistros() {
    var list_registros = sim.cpu.registros

    list_registros.foreach(registro ⇒ {
      registro.valor = new W16("0000")
    })
  }

  def cheakearInputs() {
    sim.cpu.registros.foreach(registro ⇒ {
      if (registro.valor.hex.size < 4) { throw new ModificarValorException(registro.getValorString + " No es un argumento válido para un registro.") }
    })
    sim.busIO.puertos.celdas.foreach(celda ⇒ {
      if (celda.value.hex.size < 4) { throw new ModificarValorException(celda.value.hex + " No es un argumento válido para una celda.") }
    })
    if (sim.cpu.pc.hex.size < 4) { throw new ModificarValorException(sim.cpu.pc.hex + " No es un argumento válido para pc.") }

  }

  def cambiarEdicion() {
    cheakearInputs()
    enabled = !enabled
    if (enabled) { sim.ciclo.ninguna_etapa }
    else { sim.ciclo.pasarAFetch }

  }

}

class QSimWindows(owner: WindowOwner, model: SimuladorAppmodel) extends Dialog[SimuladorAppmodel](owner, model) {

  override def createErrorsPanel(parent: Panel) = {
    this.setTaskDescription("")
    super.createErrorsPanel(parent)
  }

  override def createFormPanel(mainPanel: Panel) = {
    this.setTitle("Qsim")
    var groupPanel = new Panel(mainPanel)
      .setLayout(new VerticalLayout())
    var form = new Panel(groupPanel)
    form.setLayout(new HorizontalLayout())

    var second_form = new Panel(form)
    second_form.setLayout(new VerticalLayout())
    crearRegistrosEspeciales(second_form)
    crearFlags(second_form)
    crearPanelDeRegistros(second_form)
    crearMemoria(form)
    crearTexArea(second_form)
    crearBotonesAccion(second_form)

    //     val ver_puertos = new Button(second_form)
    //      .setCaption("Ver puertos")
    //      .onClick(new MessageSend(QSimWindows.this, "createPuertosWindow"))
  }

  def crearBotonesAccion(parent: Panel) {

    var panelBotones = new Panel(parent)
    panelBotones.setLayout(new ColumnLayout(3))

    var ciclo_pap_Form = new GroupPanel(panelBotones)
    ciclo_pap_Form.setTitle("Ejecucion Paso a Paso")
    ciclo_pap_Form.setLayout(new HorizontalLayout())

    new Button(ciclo_pap_Form)
      .setCaption(StringUtils.capitalize("Buscar"))
      .onClick(new MessageSend(model.sim, "fetch"))
      .setAsDefault
      .bindEnabled(new ObservableProperty(model.sim.ciclo, "fetch"))

    new Button(ciclo_pap_Form)
      .setCaption(StringUtils.capitalize("Decodificar"))
      .onClick(new MessageSend(model.sim, "decode"))
      .setAsDefault
      .bindEnabled(new ObservableProperty(model.sim.ciclo, "decode"))

    new Button(ciclo_pap_Form)
      .setCaption(StringUtils.capitalize("Ejecutar"))
      .onClick(new MessageSend(model.sim, "execute"))
      .setAsDefault
      .bindEnabled(new ObservableProperty(model.sim.ciclo, "execute"))

    var ciclo_compl_Form = new GroupPanel(panelBotones)
    ciclo_compl_Form.setTitle("Ejecucion Completa")
    ciclo_compl_Form.setLayout(new HorizontalLayout())

    new Button(ciclo_compl_Form)
      .setCaption(StringUtils.capitalize("Busc-Decod-Ejecu"))
      .onClick(new MessageSend(model.sim, "execute_complete"))
      .setAsDefault
      .bindEnabled(new ObservableProperty(model.sim.ciclo, "execute_complete"))

    var buttons_Form = new GroupPanel(panelBotones)
    buttons_Form.setTitle("Edicion")
    buttons_Form.setLayout(new HorizontalLayout())

    val editable = new Button(buttons_Form)
      .setCaption("Editar")
      .onClick(new MessageSend(model, "cambiarEdicion"))
  }

  def crearMemoria(parent: Panel) {
    var memoriaForm = new Panel(parent)
    memoriaForm.setLayout(new VerticalLayout())

    var buttonPanel = new Panel(memoriaForm).setLayout(new ColumnLayout(3))
    var buttonDH = new Panel(buttonPanel).setLayout(new ColumnLayout(4))
    new Label(buttonDH).setText("Desde:")
    new TextBox(buttonDH).withFilter(w16Filter).bindValueToProperty("desde.hex")
    new Label(buttonDH).setText("Hasta:")
    new TextBox(buttonDH).withFilter(w16Filter).bindValueToProperty("hasta.hex")
    var buttonAct = new Panel(buttonPanel).setLayout(new ColumnLayout(1))
    new Button(buttonAct).onClick(new MessageSend(model, "crearFila16Celdas")).setCaption("Actualizar")
    var linkPaginacion = new Panel(buttonPanel).setLayout(new ColumnLayout(4))
    new Link(linkPaginacion).setCaption("Inicio").onClick(new MessageSend(model, "paginaInicial"))
    val anterior = new Link(linkPaginacion).onClick(new MessageSend(model, "paginaAnterior"))
    anterior.bindCaptionToProperty("prev")
    anterior.bindVisibleToProperty("prevVisible")
    val siguiente = new Link(linkPaginacion).onClick(new MessageSend(model, "paginaSiguiente"))
    siguiente.bindCaptionToProperty("next")
    siguiente.bindVisibleToProperty("nextVisible")
    new Link(linkPaginacion).onClick(new MessageSend(model, "paginaFinal")).setCaption("Fin")

    var table = new Table[Fila16Celdas](memoriaForm, classOf[Fila16Celdas])
    table.setHeight(500)
    table.setWidth(850)
    table.bindItemsToProperty("celdas")
    new Column[Fila16Celdas](table) //
      .setTitle("")
      .setFixedSize(50)
      .bindContentsToProperty("name")
    for (celdas ← 0 to 15) {
      val column = new Column[Fila16Celdas](table) //
      column.setTitle(Util.IntSumToHex(celdas))
        .setFixedSize(50)
        .bindContentsToProperty(s"celda$celdas")
      column.bindBackground(s"celda$celdas.state", new Transformer[Type, Color]() {
        def transform(element: Type) = element match {
          case NONE ⇒ Color.WHITE
          case PROGRAM ⇒ Color.LIGHT_GRAY
          case STORE ⇒ Color.BLUE
          case FECH_DECODE ⇒ Color.GREEN
          case EXECUTED ⇒ Color.CYAN
          case _ ⇒ null
        }
      })
    }

  }

  val w16Filter = new TextFilter() {
    def accept(event: TextInputEvent): Boolean = {
      event.getPotentialTextResult().matches("[A-F0-9]{0,4}")
    }
  }

  def crearRegistrosEspeciales(parent: Panel) {
    var FlagsForm = new GroupPanel(parent)
    FlagsForm.setTitle("Registros especiales")
    FlagsForm.setLayout(new ColumnLayout(2))

    new Label(FlagsForm).setText("PC")
    val text_pc = editableText(FlagsForm)
    //      text_pc.bindEnabledToProperty("enabled")
    text_pc.bindValueToProperty(s"sim.cpu.pc.hex")
    text_pc.setWidth(110).setHeight(15)
    text_pc.withFilter(w16Filter)

    new Label(FlagsForm).setText("SP")
    val text_sp = disableText(FlagsForm)
    //      text_sp.bindEnabledToProperty("never_enabled")
    text_sp.bindValueToProperty(s"sim.cpu.sp.hex")
    text_sp.setWidth(110).setHeight(15)

    new Label(FlagsForm).setText("IR")
    val text = disableText(FlagsForm)
    //text.bindEnabledToProperty("never_enabled")
    text.bindValueToProperty(s"sim.cpu.ir")
    text.setWidth(110).setHeight(15)
  }

  def editableText(parent: Panel) = {
    val text = new TextBox(parent)
    text.bindEnabled(new ObservableProperty(this.getModelObject(), "enabled"))
    text
  }

  def disableText(parent: Panel) = {
    val text = new TextBox(parent)
    text.bindEnabled(new ObservableProperty(this.getModelObject(), "never_enabled"))
    text
  }

  def crearFlags(parent: Panel) {
    var FlagsForm = new GroupPanel(parent)
    FlagsForm.setTitle("Flags")
    FlagsForm.setLayout(new ColumnLayout(4))

    scala.List[String]("n", "v", "z", "c").foreach(prop ⇒ {
      new Label(FlagsForm).setText(prop.toUpperCase)
      val text = editableText(FlagsForm)
      text.bindValueToProperty(s"sim.cpu.$prop")
      text.setWidth(30).setHeight(15)
      text.withFilter(new TextFilter() {
        def accept(event: TextInputEvent): Boolean = {
          event.getPotentialTextResult().matches("[0-1]{1}")
        }
      })
    })

  }

  def crearTexArea(parent: Panel) {
    var panelForm = new Panel(parent)
    panelForm.setLayout(new HorizontalLayout())
    new TextBox(panelForm)
      .setMultiLine(true)
      .selectFinalLine()
      .setWidth(360)
      .setHeight(220)
      .bindValueToProperty("sim.mensaje_al_usuario")
  }

  def crearPanelDeRegistros(parent: Panel) {
    var registrosForm = new GroupPanel(parent)
    registrosForm.setTitle("Registros")
    registrosForm.setLayout(new ColumnLayout(4))
    model.sim.cpu.registros.foreach(registro ⇒ {
      val registroPanel = new Panel(registrosForm, registro).setLayout(new ColumnLayout(2))
      new Label(registroPanel).setText("R" + registro.numero)
      val text = editableText(registroPanel)
      text.withFilter(new TextFilter() {
        def accept(event: TextInputEvent): Boolean = {
          event.getPotentialTextResult().matches("[A-F0-9]{0,4}")
        }
      })
      //      text.bindEnabled(new ObservableProperty(this.getModelObject(), "enabled"))
      text.bindValueToProperty("valor.hex")
    })
  }

  override def addActions(actions: Panel) = {

  }

  def createPuertosWindow() = {
    (new PuertosWindow(this.getOwner(), this.getModelObject())).open()
  }

}

object QSimRunner extends Application with App {

  def createMainWindow(): Window[_] = {
    var la = new QSimMain()
    //  la.setPathArchivo("src/main/resources/programaQ1.qsim")
    //   la.setPathArchivo("src/main/resources/programaQ2.qsim")
    //    la.setPathArchivo("src/main/resources/programaQ3.qsim")
    new QSimWindow(this, la)
  }
  start()
}
