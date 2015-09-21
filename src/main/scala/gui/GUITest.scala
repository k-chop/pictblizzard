package pictbliz.gui

import org.eclipse.jface._
import org.eclipse.swt.widgets._
import org.eclipse.swt.layout._
import org.eclipse.jface.window.ApplicationWindow
import org.eclipse.swt.SWT
import text.{ITextViewer, Document, TextViewer}
import java.awt.image.BufferedImage
import org.eclipse.swt.events._


import pictbliz.scriptops.Parser
import pictbliz.{DrawableImage, Resource}
import gui.SWTUtils
import org.eclipse.swt.graphics.{Font, Image, ImageData}


object GUITest {

  def main(args: Array[String]) {

    val g = new GUITest

  }
}

class GUITest extends ApplicationWindow(null) {

  this.setBlockOnOpen(true)
  this.open()
  Display.getCurrent.dispose()

  /**
   * 各コンポーネントの配置
   * @param parent どうでもいい
   * @return どうでもいい
   */
  override protected def createContents(parent: Composite) = {

    // set-Title
    parent.getShell.setText("Pict-Stamp")
    // create root composite
    val root = new RootComposite(parent)
    // create Model
    val model = new RootModel
    // create Presenter
    val presenter = new RootPresenter(model)
    // create View
    val view = new RootView(presenter, root)
    presenter.view = view

    parent.pack()
    parent
  }

}


trait Model {
  def clearCache()
  def compileAndStore(code: String): Option[BufferedImage]
  def writeAll(path: String)
}

/**
 * GUI以外のクラスをいじくる
 * View, Presenterに依存しない
 */
class RootModel extends Model {
  protected var imgCache = collection.mutable.ListBuffer.empty[DrawableImage]

  def clearCache() {
    imgCache.clear()
  }

  def writeAll(path: String) {
    imgCache.foreach( _.write(path) )
  }

  /**
   * 渡されてきたcodeから画像を生成して一時保存する
   * 見本として1つ目の画像を返す
   * @param code スクリプト
   * @return 見本画像
   */
  def compileAndStore(code: String): Option[BufferedImage] = {
    val result = Parser.parse(code)
    clearCache()

    result foreach {
      // 画像を生成してimgCacheに格納
      case _ =>
    }
    imgCache.headOption.map { _.result }
  }
}

trait Presenter {
  var view: View
  def compileAndStore(code: String): Option[BufferedImage]
  def writeAll(path: String)
}

/**
 * Modelから色々持ってきてViewに渡す
 */
class RootPresenter(protected val model: Model) extends Presenter {
  // 初期化の順番の関係上後でからセットするしかねェ！！
  var view: View = _

  def compileAndStore(code: String): Option[BufferedImage] = model.compileAndStore(code)
  def writeAll(path: String) { model.writeAll(path) }
}

trait View {
  protected val presenter: Presenter
  val root: Composite
}

/**
 * Presenterから指示をもらって画面更新
 * ユーザ入力はPresenterへ流す
 */
class RootView(val presenter: RootPresenter, val root: RootComposite) extends View {

  private[this] var screen: Option[ImageData] = None

  def log(str: String) {
    root.logview.logview.getTextWidget.append(str + "\n")
  }

  root.exbutton.btGen.addSelectionListener(new SelectionListener {
    // コンパイル実行
    def widgetSelected(p1: SelectionEvent) {
      val str = root.inputarea.inputarea.getDocument.get()
      log("compile " + str.takeWhile(_ != '\n') + "...")
      val res = presenter.compileAndStore(str)
      log("compile end.")

      screen = res.map { r => SWTUtils.convertToSWT(r) }
      root.resview.canvas.redraw()
    }
    def widgetDefaultSelected(p1: SelectionEvent) {} // 呼び出されない
  })

  root.exbutton.btOutput.addSelectionListener(new SelectionListener {
    // 画像出力
    def widgetSelected(p1: SelectionEvent) {
      val dest = Resource.tempdir
      presenter.writeAll(dest)
      log(s"$dest に画像を出力しました.")
    }

    def widgetDefaultSelected(p1: SelectionEvent) {} // 呼び出されない
  })

  root.resview.canvas.addPaintListener(new PaintListener {
    def paintControl(p1: PaintEvent) {
      if (screen.isDefined) {
        val img = new Image(Display.getCurrent, screen.get)
        p1.gc.drawImage(img, 0, 0)
        img.dispose()
      }
    }
  })

}

class RootComposite(parent: Composite) extends Composite(parent, SWT.NONE) {

  private val rootLayout = new GridLayout(2, false)
  setLayout(rootLayout)

  // 画面の出力用 (320x240)
  val resview = new ResultView(this, SWT.NONE)
  resview.setLayoutData({
    new GridData(GridData.HORIZONTAL_ALIGN_CENTER | GridData.VERTICAL_ALIGN_CENTER)
  })

  // 出力ボタン用
  val exbutton = new ExecuteButtons(this, SWT.NONE)
  exbutton.setLayoutData({
    new GridData(GridData.HORIZONTAL_ALIGN_CENTER | GridData.VERTICAL_ALIGN_CENTER)
  })

  // テキストエリア
  val inputarea = new InputArea(this, SWT.NONE)
  inputarea.setLayoutData({
    val dat = new GridData(GridData.HORIZONTAL_ALIGN_CENTER | GridData.VERTICAL_ALIGN_CENTER | GridData.FILL_BOTH)
    dat.horizontalSpan = 2
    dat
  })

  // ログ出力
  val logview = new LogView(this, SWT.NONE)
  logview.setLayoutData({
    val dat = new GridData(GridData.HORIZONTAL_ALIGN_CENTER | GridData.VERTICAL_ALIGN_BEGINNING | GridData.FILL_BOTH)
    dat.horizontalSpan = 2
    dat
  })

  pack()

}

class ResultView(parent: Composite, style: Int) extends Composite(parent, style) {
  val resviewG = new Group(this, SWT.SHADOW_OUT | SWT.SHADOW_IN)
  resviewG.setText("生成結果")
  resviewG.setSize(340, 270)

  val canvas = new Canvas(resviewG, SWT.BORDER_DASH)
  canvas.setBounds(0, 0, 320, 240)
  canvas.setSize(320, 240)
  canvas.setLocation(10,20)

  setSize(340, 270)
}

class ExecuteButtons(parent: Composite, style: Int) extends Composite(parent, style) {
  val lay = new GridLayout(1, false)
  setLayout(lay)

  val btGen = new Button(this, SWT.PUSH)
  btGen.setLayoutData({
    import GridData._
    val dat = new GridData(FILL_HORIZONTAL | VERTICAL_ALIGN_BEGINNING)
    dat.verticalIndent = 8
    dat
  })
  btGen.setText("プレビュー")
  btGen.pack()

  val btOutput = new Button(this, SWT.PUSH)
  btOutput.setLayoutData({
    import GridData._
    val dat = new GridData(FILL_HORIZONTAL | VERTICAL_ALIGN_BEGINNING)
    dat.verticalIndent = 8
    dat
  })
  btOutput.setText("画像をすべて出力")
  btOutput.pack()

  setSize(120, 240)
}

class InputArea(parent: Composite, style: Int) extends Composite(parent, style) {
  val inG = new Group(this, SWT.SHADOW_OUT | SWT.SHADOW_IN)
  inG.setText("スクリプト入力")
  inG.setSize(500, 230)

  val inputarea: ITextViewer = new TextViewer(inG, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER)
  inputarea.setDocument(new Document("""val b = layout (size(320,240)) {
                                       |  name: { rect(5,5,300,20), auto_expand }
                                       |  icon: { rect(280, 0, 32, 32) }
                                       |  desc: {
                                       |    rect(0,20,320,120),
                                       |    window("system6.png"),
                                       |    auto_expand,
                                       |    front_color("system6.png"),
                                       |    interval(0,2),
                                       |    padding(8,9),
                                       |    align("x_center", "y_center")
                                       |  }
                                       |  cost: { rect(300,2,30,15), font("Verdana", "plain", 10) }
                                       |}
                                       |
                                       |val a = values {
                                       |  id  : 1
                                       |  name: "エターナルフォースブリザード"
                                       |  desc: "一瞬で相手の周囲の大気ごと氷結させる\n相手は死ぬ"
                                       |  cost: "42"
                                       |  icon: icon("icon/icon1.png")
                                       |  filename: "#{id}_#{name}"
                                       |}
                                       |
                                       |generate b with a
                                       |""".stripMargin))
  inputarea.getTextWidget.setLocation(10,20)
  inputarea.getTextWidget.setSize(480,200)
}

class LogView(parent: Composite, style: Int) extends Composite(parent, style) {
  val logG = new Group(this, SWT.SHADOW_OUT | SWT.SHADOW_IN)
  logG.setText("ログ")
  logG.setSize(500, 130)

  val logview: ITextViewer = new TextViewer(logG, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER)
  logview.setDocument(new Document(""))
  logview.setEditable(false)
  logview.getTextWidget.setLocation(10,20)
  logview.getTextWidget.setSize(480,100)
  logview.getTextWidget.addModifyListener(new ModifyListener {
    def modifyText(p1: ModifyEvent) {
      logview.getTextWidget.setTopIndex(logview.getTextWidget.getLineCount - 1)
    }
  })
}

