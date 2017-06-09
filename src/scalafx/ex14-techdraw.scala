package oka.ex14

import scala.math.{min,abs,atan2,sin,cos,sqrt,Pi}
import scala.collection.mutable.{Buffer, Map}
import scala.compat.Platform

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.event.ActionEvent
import scalafx.geometry.Point2D
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ButtonBase, ColorPicker, ToggleButton, ToolBar, ToggleGroup, MenuButton, MenuItem}
import scalafx.scene.effect.DropShadow
import scalafx.scene.input.{KeyEvent, MouseEvent}
import scalafx.scene.layout.{Pane, BorderPane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Ellipse, Line, Polygon, Polyline, Rectangle, Shape}

abstract class TDShape
case class TDNoShape  ()                extends TDShape
case class TDLine     (shape:Line)      extends TDShape
case class TDRectangle(shape:Rectangle) extends TDShape
case class TDEllipse  (shape:Ellipse)   extends TDShape
case class TDPolyline (shape:Polyline)  extends TDShape
case class TDFreeline (shape:Polyline)  extends TDShape
case class TDPolygon  (shape:Polygon)   extends TDShape
case class TDStar     (shape:Polygon)   extends TDShape
case class TDHinomaru (flag:Rectangle, disc:Ellipse)    extends TDShape

object TechDraw extends JFXApp {

	var shape: TDShape = TDNoShape()
	var strokeColor: Color = Color.Black
	var fillColor: Color   = Color.White
	var stroke_width: Int   = 3
	var shapes: Buffer[TDShape] = Buffer()

	val canvas = new Pane { }
	val DOUBLE_CLICK_WAIT_MS = 300; // milliseconds
	var clickedAt = Long.MinValue

	type MouseHandler = MouseEvent => Unit

	object mouseAction {
		val _noAction:  MouseHandler = { _ => () }
		var onPressed:  MouseHandler = _noAction
		var onDragged:  MouseHandler = _noAction
		var onReleased: MouseHandler = _noAction
		var onMoved  :  MouseHandler = _noAction

		var pressHandlers   = Map[String, MouseHandler]()
		var dragHandlers    = Map[String, MouseHandler]()
		var releaseHandlers = Map[String, MouseHandler]()
		var moveHandlers    = Map[String, MouseHandler]()

		def set(id: String) {
			def lookup(handlers: Map[String, MouseHandler]) = {
				try { handlers(id) } catch { case _: NoSuchElementException => _noAction }
			}

			drawingPane.onMousePressed  = lookup(pressHandlers)
			drawingPane.onMouseDragged  = lookup(dragHandlers)
			drawingPane.onMouseReleased = lookup(releaseHandlers)
			drawingPane.onMouseMoved    = lookup(moveHandlers)
		}

		def reset {
			drawingPane.onMousePressed  = _noAction
			drawingPane.onMouseDragged  = _noAction
			drawingPane.onMouseReleased = _noAction
			drawingPane.onMouseMoved    = _noAction
		}
	}

	object RectangleControl { // 矩形の描画に関わるデータ構造と処理

		var r  = new Rectangle {}
		var p0 = new Point2D(0, 0)

		def onPress(ev: MouseEvent) {
			p0 = new Point2D(ev.x, ev.y)
			r = new Rectangle {
				x = p0.x; y = p0.y
				stroke = Color.Blue
				fill = Color.Transparent
				strokeWidth = stroke_width
			}
			shape = TDRectangle(r)
			drawingPane.content += r
			shapes += shape
		}

		def onDrag(ev: MouseEvent) {
			r.x     = min(p0.x, ev.x);  r.y      = min(p0.y, ev.y)
			r.width = abs(p0.x - ev.x); r.height = abs(p0.y - ev.y)
		}

		def onRelease(ev: MouseEvent) {
			r.fill   = fillColor
			r.stroke = strokeColor
		}
	}

	mouseAction.pressHandlers   += (("Rectangle", RectangleControl.onPress))
	mouseAction.dragHandlers    += (("Rectangle", RectangleControl.onDrag))
	mouseAction.releaseHandlers += (("Rectangle", RectangleControl.onRelease))

	object EllipseControl { // 楕円の描画に関わるデータ構造と処理

		var e = new Ellipse {}
		var p0 = new Point2D(0, 0)

		def onPress(ev: MouseEvent) {
			p0 = new Point2D(ev.x, ev.y)
			e = new Ellipse {
				centerX = p0.x; centerY = p0.y
				stroke = Color.Blue
				fill = Color.Transparent
				strokeWidth = stroke_width
			}
			shape = TDEllipse(e)
			drawingPane.content += e
			shapes += shape
		}

		def onDrag(ev: MouseEvent) {
			e.centerX = (p0.x + ev.x) / 2;  e.centerY = (p0.y + ev.y) / 2
			e.radiusX = abs(p0.x - e.centerX());  e.radiusY = abs(p0.y - e.centerY())
		}

		def onRelease(ev: MouseEvent) {
			e.fill   = fillColor
			e.stroke = strokeColor
		}
	}

	mouseAction.pressHandlers   += (("Ellipse", EllipseControl.onPress))
	mouseAction.dragHandlers    += (("Ellipse", EllipseControl.onDrag))
	mouseAction.releaseHandlers += (("Ellipse", EllipseControl.onRelease))

	object LineControl { // 線分の描画に関わるデータ構造と処理

		var l  = new Line {}
		var p0 = new Point2D(0, 0)

		def onPress(ev: MouseEvent) {
			p0 = new Point2D(ev.x, ev.y)
			l  = new Line {
				stroke = Color.Blue; fill = Color.Transparent; strokeWidth = stroke_width
			}
			shape = TDLine(l)
			drawingPane.content += l
			shapes += shape
			l.startX = p0.x; l.startY = p0.y
			l.endX   = ev.x; l.endY   = ev.y
		}

		def onDrag(ev: MouseEvent) {
			l.startX = p0.x; l.startY = p0.y
			l.endX   = ev.x; l.endY   = ev.y
		}

		def onRelease(ev: MouseEvent) {
			l.fill   = fillColor
			l.stroke = strokeColor
		}
	}

	mouseAction.pressHandlers   += (("Line", LineControl.onPress))
	mouseAction.dragHandlers    += (("Line", LineControl.onDrag))
	mouseAction.releaseHandlers += (("Line", LineControl.onRelease))

	object PolylineControl {
		var firstclick = true // 次回クリックが折れ線開始時の場合 true 折れ線作成途中なら false
		var p = new Polyline {}
		var l = new Line {}

		def reset() { firstclick = true; drawingPane.content -= l}

		def onPress(ev: MouseEvent) {
			if (firstclick) { // 初回クリックの場合
				p = new Polyline {stroke = strokeColor; fill = Color.Transparent; strokeWidth = stroke_width}
				l = new Line {stroke = Color.Blue; fill = Color.Transparent; strokeWidth = stroke_width}
				shape = TDPolyline(p)
				drawingPane.content += p += l
				shapes += shape
				firstclick = false
				p.points ++= List(ev.x, ev.y)
				l.endX = ev.x; l.endY = ev.y
			}
			else if (!firstclick && click >= 2) click = 0
			else {
				if (!firstclick){
					if (p.points.init.last != ev.x || p.points.last != ev.y) {
						p.points ++= List(ev.x,ev.y)
					}
					else reset
				}
			}
			l.startX = ev.x; l.startY = ev.y
		}

		def onMove(ev: MouseEvent) {
			l.endX = ev.x; l.endY = ev.y
		}
	}

	mouseAction.pressHandlers += (("Polyline", PolylineControl.onPress))
	mouseAction.moveHandlers  += (("Polyline", PolylineControl.onMove))

	object FreeLineControl {
		var f = new Polyline {}

		def onPress(ev: MouseEvent) {
			f = new Polyline {stroke = Color.Blue; fill = Color.Transparent; strokeWidth = stroke_width}
			shape = TDFreeline(f)
			drawingPane.content += f
			shapes += shape
			f.points ++= List(ev.x,ev.y)
		}

		def onDrag(ev: MouseEvent) {
			f.points ++= List(ev.x, ev.y)
		}

		def onRelease(ev: MouseEvent) {
			f.points ++= List(ev.x, ev.y)
			f.stroke = strokeColor
		}
	}

	mouseAction.pressHandlers   += (("FreeLine", FreeLineControl.onPress))
	mouseAction.dragHandlers    += (("FreeLine", FreeLineControl.onDrag))
	mouseAction.releaseHandlers += (("FreeLine", FreeLineControl.onRelease))

	object SelectControl { // 選択ツールに関わるデータ構造と処理
		var selection: TDShape = TDNoShape()
		var p0 = new Point2D(0, 0)
		var dataOfShape: List[Double] = Nil // 図形の情報を保存しておく変数
		var px: List[Double] = Nil
		var circles    : List[Ellipse] = Nil // 作成した制御点を置いておく変数
		var circles_sub: List[Ellipse] = Nil // 選択されている図形に対する制御点のリスト
		var rotate_line   = new Line {} // 回転の制御点と図形をつなぐ線分
		var bool = -1 // 制御点ドラッグ時、どの制御点を持っているのかを示す指標。制御点を持っていない場合は bool = -1

		/**
		 * 点cを中心に点qをthetaだけ回転した点を返す関数
		 * ただし、theta は ラジアンとする
		 */
		def rotatePoint(q: Point2D, c: Point2D, theta: Double): Point2D = {
			new Point2D(c.x + q.distance(c) * sin(theta - atan2(c.x - q.x, c.y - q.y)),
				        c.y - q.distance(c) * cos(theta - atan2(c.x - q.x, c.y - q.y)))
		}

		// 角度の単位を変換する関数。
		def radianToDegree(theta: Double): Double = {theta * 180 / Pi}
		def degreeToRadian(theta: Double): Double = {theta * Pi / 180}

		// 関数 setcircles 内で計算の為に使う変数
		var dataOfCircles: List[Point2D] = Nil

		// 図形の制御点となる円を、もとの図形の形に合わせて設置する関数。
		def setcircles(shape: TDShape) = {
			shape match{
				case TDRectangle(r) => {
					val center = new Point2D(r.x() + r.width() / 2, r.y() + r.height() / 2)
					dataOfCircles = Nil
					dataOfCircles ++= List(rotatePoint(new Point2D(center.x         , r.y() - 40        ), center, degreeToRadian(r.rotate())),
						                   rotatePoint(new Point2D(r.x()            , r.y()             ), center, degreeToRadian(r.rotate())),
					                       rotatePoint(new Point2D(r.x() + r.width(), r.y()             ), center, degreeToRadian(r.rotate())),
					                       rotatePoint(new Point2D(r.x()            , r.y() + r.height()), center, degreeToRadian(r.rotate())),
					                       rotatePoint(new Point2D(r.x() + r.width(), r.y() + r.height()), center, degreeToRadian(r.rotate())))
					for(i <- 0 to 4) {
						circles(i).centerX = dataOfCircles(i).x ; circles(i).centerY = dataOfCircles(i).y
					}
					// 回転の制御点の線分も設置する
					rotate_line.startX = (circles(1).centerX()+ circles(2).centerX()) / 2
					rotate_line.startY = (circles(1).centerY() + circles(2).centerY()) / 2
					rotate_line.endX   = circles(0).centerX()
					rotate_line.endY= circles(0).centerY()
				}
				case TDEllipse(e)   => {
					val center = new Point2D(e.centerX(), e.centerY())
					dataOfCircles = Nil
					dataOfCircles ++= List(rotatePoint(new Point2D(center.x              , center.y - e.radiusY() - 40), center, degreeToRadian(e.rotate())),
						                   rotatePoint(new Point2D(center.x              , center.y - e.radiusY()     ), center, degreeToRadian(e.rotate())),
					                       rotatePoint(new Point2D(center.x - e.radiusX(), center.y                   ), center, degreeToRadian(e.rotate())),
					                       rotatePoint(new Point2D(center.x + e.radiusX(), center.y                   ), center, degreeToRadian(e.rotate())),
					                       rotatePoint(new Point2D(center.x              , center.y + e.radiusY()     ), center, degreeToRadian(e.rotate())))
					for (i <- 0 to 4) {
						circles(i).centerX = dataOfCircles(i).x ; circles(i).centerY = dataOfCircles(i).y
					}
					// 回転の制御点の線分も設置する
					rotate_line.startX = circles(1).centerX()
					rotate_line.startY = circles(1).centerY()
					rotate_line.endX   = circles(0).centerX()
					rotate_line.endY   = circles(0).centerY()
				}
				case TDLine(l)      => {
					circles(0).centerX = l.startX()  ; circles(0).centerY = l.startY()
					circles(1).centerX = l.endX()    ; circles(1).centerY = l.endY()
				}
				case TDPolyline(p)  => {
					for(i <- 0 to p.points.length / 2 - 1) {
						circles(i).centerX = p.points(2 * i).toDouble
						circles(i).centerY = p.points(2 * i + 1).toDouble
					}
				}
				case _              => {}
			}
		}

		// 生成したい制御点の個数を引数として受け取り、その個数分だけ circles に加える関数
		def make_circles(n: Int) = {
			for (i <- 1 to n) {
				circles ++= List(new Ellipse {radiusX = 5; radiusY = 5; fill = Color.White; stroke = Color.Black})
			}
		}
		make_circles(5) // 長方形、楕円、線分に使用する最低数の制御点を予め作成しておく

		// 選択されている図形を選択されていない状態にする関数
		def reset_selection = {
			for (i <- 0 to shapes.length - 1) { // 一旦全ての図形のエフェクトを消去する。
				shapes(i) match {
					case TDRectangle(r) => r.effect = null
					case TDEllipse(e)   => e.effect = null
					case TDLine(l)      => l.effect = null
					case TDPolyline(p)  => p.effect = null
					case TDFreeline(f)  => f.effect = null
				}
			}
			selection = TDNoShape()
			circles_sub.foreach { drawingPane.content -= _ } // キャンバスから制御点の円を消去
			drawingPane.content -= rotate_line
			circles_sub = Nil
		}

		def onPress(ev: MouseEvent) {
			p0 = new Point2D(ev.x, ev.y)
			val x = ev.x; val y = ev.y
			if (circles_sub.filter(_.contains(x, y)).isEmpty){ // クリックした点が制御点上でなかった場合のみ行う。制御点をクリックした際は選択図形に変化はない。
				reset_selection
				val oShape = shapes.reverse.find((shape: TDShape) =>
					shape match {
						case TDRectangle(r) => {
							val t = rotatePoint(p0, new Point2D(r.x() + r.width() / 2, r.y() + r.height() / 2), -degreeToRadian(r.rotate()))
							r.contains(t.x, t.y)
						}
						case TDEllipse(e)   => {
							val t = rotatePoint(p0, new Point2D(e.centerX(), e.centerY()), -degreeToRadian(e.rotate()))
							e.contains(t.x, t.y)
						}
						case TDLine(l)      => l.contains(x, y)
						case TDPolyline(p)  => p.contains(x, y)
						case TDFreeline(f)  => f.contains(x, y)
					})
				selection = oShape match {
					case Some(shape) => shape
					case _           => TDNoShape()
				}
			}
			circles_sub.foreach { drawingPane.content -= _ } // キャンバスから制御点の円を消去
			drawingPane.content -= rotate_line
			circles_sub = Nil // この時点では選択されている図形と circles_sub の内容が異なっているためリセット
			circles(0).fill = Color.White // 回転機能を伴う図形選択後では、circles(0)の色はオレンジになっているので白に変更
			selection match {
				case TDRectangle(r) => { // 選択されている図形が長方形の場合
					r.effect = new DropShadow(10, Color.Blue)
					drawingPane.content -= r += r // 選択された図形をキャンバスの先頭にもってくる
					shapes -= selection += selection
					dataOfShape = List(r.x(), r.y(), r.width(), r.height(), r.rotate()) // データを保存
					circles_sub = circles.take(5) // 制御点となる円を４つ取り出す
					drawingPane.content += rotate_line
					circles_sub.foreach { drawingPane.content += _ } // 制御点をキャンバスに追加
					circles(0).fill = Color.Orange // 回転の制御点をオレンジにする
					strokeWidthTools(0).text = f"${r.strokeWidth().toInt}" // ツールボタンの太さの欄を、この長方形の線の太さを書き込む
					stroke_width = r.strokeWidth().toInt
					// 1  2
					// 3  4  のように四角形の四隅に番号付けをする
					for(i <- 0 to 4){
						if(circles(i).contains(x, y)) bool = i
					}
				}
				case TDEllipse(e)   => { // 選択されている図形が楕円の場合
					e.effect = new DropShadow(10, Color.Blue)
					drawingPane.content -= e += e
					shapes -= selection += selection
					dataOfShape = List(e.centerX(), e.centerY(), e.radiusX(), e.radiusY(), e.rotate())
					circles_sub = circles.take(5)
					drawingPane.content += rotate_line
					circles_sub.foreach { drawingPane.content += _ }
					circles(0).fill = Color.Orange
					strokeWidthTools(0).text = f"${e.strokeWidth().toInt}"
					stroke_width = e.strokeWidth().toInt
					//    1
					// 2     3  のように、楕円の四隅に番号付けをする
					//    4
					for(i <- 0 to 4){
						if(circles(i).contains(x, y)) bool = i
					}
				}
				case TDLine(l)      => { // 選択されている図形が線分の場合
					l.effect = new DropShadow(10, Color.Blue)
					drawingPane.content -= l += l
					shapes -= selection += selection
					dataOfShape = List(l.startX(), l.startY(), l.endX(), l.endY())
					circles_sub = circles.take(2)
					circles_sub.foreach { drawingPane.content += _ }
					strokeWidthTools(0).text = f"${l.strokeWidth().toInt}"
					stroke_width = l.strokeWidth().toInt
					// 線の一方を 0 もう一方を 1 とする
					if     (circles(0).contains(x, y)) bool = 0
					else if(circles(1).contains(x, y)) bool = 1
				}
				case TDPolyline(p)  => { // 選択されている図形が折れ線の場合
					p.effect = new DropShadow(10, Color.Blue)
					drawingPane.content -= p += p
					shapes -= selection += selection
					dataOfShape = List(x, y)
					make_circles(p.points.length / 2 - circles.length) // 足りていない制御点を生成
					circles_sub = circles.take(p.points.length / 2)
					circles_sub.foreach { drawingPane.content += _ }
					strokeWidthTools(0).text = f"${p.strokeWidth().toInt}"
					stroke_width = p.strokeWidth().toInt
					// 線を描いた順に 0 1 2 ... と番号をつける
					for(i <- 0 to circles_sub.length - 1) {
						if(circles(i).contains(x, y)) bool = i
					}
				}
				case TDFreeline(f)  => { // 選択されている図形が自由曲線の場合
					f.effect = new DropShadow(10, Color.Blue)
					drawingPane.content -= f += f
					shapes -= selection += selection
					dataOfShape = List(x, y)
					strokeWidthTools(0).text = f"${f.strokeWidth().toInt}"
					stroke_width = f.strokeWidth().toInt
				}
				case TDNoShape()    => {}
			}
			setcircles(selection)
		}

		def onDrag(ev: MouseEvent) {
			selection match {
				case TDRectangle(r) => {
					val t = rotatePoint(new Point2D(ev.x, ev.y), new Point2D(dataOfShape(0) + dataOfShape(2) / 2, dataOfShape(1) + dataOfShape(3) / 2), -degreeToRadian(r.rotate()))
					if     (bool == 0) r.rotate = radianToDegree(Pi - atan2(ev.x - (dataOfShape(0) + dataOfShape(2) / 2), ev.y - (dataOfShape(1) + dataOfShape(3) / 2)))
					else if(bool >= 1 && bool <= 4){ // 形を変える制御点を持っている場合
						val fixed_point: Point2D = // bool == 1 のとき、左上の制御点を持っているため、右下を固定点をして定める。
							if     (bool == 1) new Point2D(dataOfShape(0) + dataOfShape(2), dataOfShape(1) + dataOfShape(3))
							else if(bool == 2) new Point2D(dataOfShape(0), dataOfShape(1) + dataOfShape(3))
							else if(bool == 3) new Point2D(dataOfShape(0) + dataOfShape(2), dataOfShape(1))
							else new Point2D(dataOfShape(0), dataOfShape(1))
						r.x = min(t.x, fixed_point.x)
						r.y = min(t.y, fixed_point.y)
						r.width  = abs(fixed_point.x - t.x)
						r.height = abs(fixed_point.y - t.y)
						// 中心が変化することで生じる固定点のズレを修正
						val before = rotatePoint(fixed_point, new Point2D(dataOfShape(0) + dataOfShape(2) / 2, dataOfShape(1) + dataOfShape(3) / 2), degreeToRadian(dataOfShape(4)))
						val after  = rotatePoint(fixed_point, new Point2D(r.x()          + r.width() / 2     , r.y()          + r.height() / 2    ), degreeToRadian(dataOfShape(4)))
						r.x = r.x() + (before.x - after.x); r.y = r.y() + (before.y - after.y)
					}
					else {r.x = dataOfShape(0) + ev.x - p0.x; r.y = dataOfShape(1) + ev.y - p0.y} // ドラッグしている点が制御点でない場合
				}
				case TDEllipse(e)   => {
					val t = rotatePoint(new Point2D(ev.x, ev.y), new Point2D(dataOfShape(0), dataOfShape(1)), -degreeToRadian(e.rotate()))
					if     (bool == 0) e.rotate = radianToDegree(Pi - atan2(ev.x - dataOfShape(0), ev.y - dataOfShape(1)))
					else if(bool <= 4 && bool >= 1){
						val fixed_point: Point2D =
							if     (bool == 1) rotatePoint(new Point2D(dataOfShape(0), dataOfShape(1) + dataOfShape(3)), new Point2D(dataOfShape(0), dataOfShape(1)), degreeToRadian(dataOfShape(4)))
							else if(bool == 2) rotatePoint(new Point2D(dataOfShape(0) + dataOfShape(2), dataOfShape(1)), new Point2D(dataOfShape(0), dataOfShape(1)), degreeToRadian(dataOfShape(4)))
							else if(bool == 3) rotatePoint(new Point2D(dataOfShape(0) - dataOfShape(2), dataOfShape(1)), new Point2D(dataOfShape(0), dataOfShape(1)), degreeToRadian(dataOfShape(4)))
							else               rotatePoint(new Point2D(dataOfShape(0), dataOfShape(1) - dataOfShape(3)), new Point2D(dataOfShape(0), dataOfShape(1)), degreeToRadian(dataOfShape(4)))
						val p_rotate = rotatePoint(new Point2D(ev.x, ev.y), fixed_point, Pi / 2 - degreeToRadian(e.rotate()))
						val H =
							if(bool == 1 || bool == 4) rotatePoint(new Point2D(p_rotate.x, fixed_point.y), fixed_point, - Pi / 2 + degreeToRadian(e.rotate()))
							else                       rotatePoint(new Point2D(fixed_point.x, p_rotate.y), fixed_point, - Pi / 2 + degreeToRadian(e.rotate()))
						e.centerX = (fixed_point.x + H.x) / 2
						e.centerY = (fixed_point.y + H.y) / 2
						if(bool == 1 || bool == 4) e.radiusY = new Point2D(e.centerX(), e.centerY()).distance(fixed_point)
						else                       e.radiusX = new Point2D(e.centerX(), e.centerY()).distance(fixed_point)
					}
					else               { // 平行移動の場合
						e.centerX = dataOfShape(0) + ev.x - p0.x
						e.centerY = dataOfShape(1) + ev.y - p0.y
					}
				}
				case TDLine(l)      => {
					if     (bool == 0) { l.startX = ev.x; l.startY = ev.y}
					else if(bool == 1) { l.endX   = ev.x; l.endY   = ev.y}
					else {
						l.startX = dataOfShape(0) + ev.x - p0.x; l.startY = dataOfShape(1) + ev.y - p0.y
						l.endX   = dataOfShape(2) + ev.x - p0.x; l.endY   = dataOfShape(3) + ev.y - p0.y
					}
				}
				case TDPolyline(p)  => {
					p.points.clear
					for (i <- 0 to circles_sub.length - 1) { // 図形変更。変更でない場合はこのループに入っても変化はない
						if (bool == i) {p.points ++= List(ev.x, ev.y)}
						else {p.points ++= List(circles_sub(i).centerX(), circles_sub(i).centerY())}
					}
					if (bool == -1) {// 平行移動
						px = Nil
						for (i <- 1 to p.points.length / 2) {
							px ++= List(p.points(2 * i - 2).toDouble + ev.x - p0.x)
							px ++= List(p.points(2 * i - 1).toDouble + ev.y - p0.y)
						}
						p.points.clear
						p0 = new Point2D(ev.x, ev.y)
						for (i <- 0 to px.length - 1) p.points ++= List(px(i))
					}
				}
				case TDFreeline(f)  => {
					px = Nil
					for (i <- 1 to f.points.length / 2) {
						px ++= List(f.points(2 * i - 2).toDouble + ev.x - p0.x)
						px ++= List(f.points(2 * i - 1).toDouble + ev.y - p0.y)
					}
					f.points.clear
					p0 = new Point2D(ev.x, ev.y)
					for (i <- 0 to px.length - 1) f.points ++= List(px(i))
				}
				case TDNoShape()    => {}
			}
			setcircles(selection)
		}

		def onRelease(ev: MouseEvent) {
			bool = -1 // bool をリセット
		}
	}

	mouseAction.pressHandlers   += (("Select", SelectControl.onPress))
	mouseAction.dragHandlers    += (("Select", SelectControl.onDrag))
	mouseAction.releaseHandlers += (("Select", SelectControl.onRelease))

	def keyevent(ev: KeyEvent) = {
		if(ev.code.name == "Delete" || ev.code.name == "Backspace"){ // 図形の削除
			shapes -= SelectControl.selection
			SelectControl.selection match {
				case TDRectangle(r) => drawingPane.content -= r
				case TDEllipse(e)   => drawingPane.content -= e
				case TDLine(l)      => drawingPane.content -= l
				case TDPolyline(p)  => drawingPane.content -= p; PolylineControl.reset
				case TDFreeline(f)  => drawingPane.content -= f
				case TDNoShape()    => {}
			}
			SelectControl.circles.foreach{ drawingPane.content -= _ }
			drawingPane.content -= SelectControl.rotate_line
		}
		else if(ev.code.name == "U" || ev.code.name == "D"){ // 図形の上下反転
			SelectControl.selection match {
				case TDRectangle(r) => r.rotate = 180 - r.rotate()
				case TDEllipse(e)   => e.rotate = 180 - e.rotate()
				case TDLine(l)      => {
					val startY = l.startY()
					l.startY = l.endY()
					l.endY = startY
				}
				case TDPolyline(p)  => {
					var x: List[Double] = Nil
					var y: List[Double] = Nil
					for (i <- 1 to p.points.length/2){ // PolyLine の構成要素を x, y 成分に分解
						x ++= List(p.points(2*i - 2).toDouble)
						y ++= List(p.points(2*i - 1).toDouble)
					}
					val center_y = (y.max + y.min) / 2 // y 座標の中心で反転
					p.points.clear
					for (i <- 1 to x.length) {
						p.points ++= List(x(i - 1))
						p.points ++= List(-y(i - 1) + 2 * center_y)
					}
				}
				case TDFreeline(f)  => {
					var x: List[Double] = Nil
					var y: List[Double] = Nil
					for (i <- 1 to f.points.length/2){ // PolyLine の構成要素を x, y 成分に分解
						x ++= List(f.points(2*i - 2).toDouble)
						y ++= List(f.points(2*i - 1).toDouble)
					}
					val center_y = (y.max + y.min) / 2
					f.points.clear
					for (i <- 1 to x.length) {
						f.points ++= List(x(i - 1))
						f.points ++= List(-y(i - 1) + 2 * center_y)
					}
				}
				case TDNoShape()    => {}
			}
			SelectControl.setcircles(SelectControl.selection)
		}
		else if(ev.code.name == "L" || ev.code.name == "R") { // 図形の左右反転
			SelectControl.selection match {
				case TDRectangle(r) => r.rotate = -r.rotate()
				case TDEllipse(e)   => e.rotate = -e.rotate()
				case TDLine(l)      => {
					var startY = l.startY()
					l.startY = l.endY()
					l.endY = startY
				}
				case TDPolyline(p)  => {
					var x: List[Double] = Nil
					var y: List[Double] = Nil
					for (i <- 1 to p.points.length/2){
						x ++= List(p.points(2*i - 2).toDouble)
						y ++= List(p.points(2*i - 1).toDouble)
					}
					val center_x = (x.max + x.min)/2 // x 座標の中心で反転
					p.points.clear
					for (i <- 1 to x.length) {
						p.points ++= List(-x(i - 1) + 2 * center_x)
						p.points ++= List(y(i - 1))
					}
				}
				case TDFreeline(f)  => {
					var x: List[Double] = Nil
					var y: List[Double] = Nil
					for (i <- 1 to f.points.length/2){
						x ++= List(f.points(2*i - 2).toDouble)
						y ++= List(f.points(2*i - 1).toDouble)
					}
					val center_x = (x.max + x.min)/2
					f.points.clear
					for (i <- 1 to x.length) {
						f.points ++= List(-x(i - 1) + 2 * center_x)
						f.points ++= List(y(i - 1))
					}
				}
				case TDNoShape()    => {}
			}
			SelectControl.setcircles(SelectControl.selection)
		}
	}

	val drawingPane = new Pane { }

	val shapeGroup = new ToggleGroup()

	shapeGroup.selectedToggle.onChange {
		if (shapeGroup.selectedToggle() != null) { // ツールボタンが選択されている状態の場合
			val id = shapeGroup.selectedToggle().asInstanceOf[javafx.scene.control.ToggleButton].id()
			mouseAction.set(id)
		}
		// ボタンの選択を解除した場合
		else mouseAction.reset
	}

	val shapeTools: List[ToggleButton] = List(
		new ToggleButton {
			id = "Select"; text = "選択"
			// 上のボタンを押したときに Polyline を終了させ、選択を解除する。
			onAction = { e: ActionEvent =>
				PolylineControl.reset
				SelectControl.reset_selection
			}
			graphic = new Rectangle { width = 0; height = 32; fill = Color.Transparent }
			toggleGroup = shapeGroup
			minWidth = 40; minHeight = 40
		},

		new ToggleButton {
			id = "Rectangle"
			onAction = { e: ActionEvent =>
				PolylineControl.reset
				SelectControl.reset_selection
			}
			graphic = new Rectangle {
				stroke = Color.Black; fill = Color.White
				width = 32; height = 32
			}
			toggleGroup = shapeGroup
		},

		new ToggleButton {
			id = "Ellipse"
			onAction = { e: ActionEvent =>
				PolylineControl.reset
				SelectControl.reset_selection
			}
			graphic = new Ellipse {
				stroke = Color.Black; fill = Color.White
				radiusX = 16; radiusY = 16
			}
			toggleGroup = shapeGroup
		},

		new ToggleButton {
			id = "Line"
			onAction = { e: ActionEvent =>
				PolylineControl.reset
				SelectControl.reset_selection
			}
			graphic = new Line {
				stroke = Color.Black; strokeWidth = 3
				startX = 0; startY = 0; endX = 28;  endY = 28
			}
			toggleGroup = shapeGroup
		},

		new ToggleButton {
			id = "Polyline"
			onAction = { e: ActionEvent =>
				PolylineControl.reset
				SelectControl.reset_selection
			}
			graphic = new Polyline {
				points ++= List( 0, 0,
								10,30,
								20, 0,
								30,30)
			}
			toggleGroup = shapeGroup
		},

		new ToggleButton {
			id = "FreeLine"
			onAction = { e: ActionEvent =>
				PolylineControl.reset
				SelectControl.reset_selection
			}
			graphic = new Polyline {
				def f(x: Double) = (25.0 / 1000.0) * x * (x - 20) * (x - 20)
				for(x <- 0 to 27){
					points ++= List(x, f(x))
				}
			}
			toggleGroup = shapeGroup
		}
	)

	val strokeWidthTools = Seq(new MenuButton(f"${stroke_width}"))

	for (i <- 1 to 10) { // 図形の線の太さ 1 ... 10 の範囲で変更可能
		val item = new MenuItem(f"${i}") {
			onAction = handle {
				stroke_width = i
				strokeWidthTools(0).text = f"${i}"
				SelectControl.selection match {
					case TDRectangle(r) => r.strokeWidth = stroke_width
					case TDEllipse(e)   => e.strokeWidth = stroke_width
					case TDLine(l)      => l.strokeWidth = stroke_width
					case TDPolyline(p)  => p.strokeWidth = stroke_width
					case TDFreeline(f)  => f.strokeWidth = stroke_width
					case _              => ()
				}
				PolylineControl.reset
			}
		}
		strokeWidthTools(0).items += item
	}

	val colorTools = Seq(
		new ColorPicker(strokeColor) {
			onAction = { e: ActionEvent =>
				val c = value()
				strokeColor = Color.hsb(c.hue, c.saturation, c.brightness, 0.5)
				SelectControl.selection match {
					case TDRectangle(r) => r.stroke = strokeColor
					case TDEllipse(e)   => e.stroke = strokeColor
					case TDLine(l)      => l.stroke = strokeColor
					case TDPolyline(p)  => p.stroke = strokeColor
					case TDFreeline(f)  => f.stroke = strokeColor
					case _              => ()
				}
			}
		},

		new ColorPicker(fillColor) {
			onAction = { e: ActionEvent =>
				val c = value()
				fillColor = Color.hsb(c.hue, c.saturation, c.brightness, 0.5)
				SelectControl.selection match {
					case TDRectangle(r) => r.fill = fillColor
					case TDEllipse(e)   => e.fill = fillColor
					case TDLine(l)      => l.fill = fillColor
					case TDPolyline(p)  => p.fill = fillColor
					case TDFreeline(f)  => f.fill = fillColor
					case _              => ()
				}
			}
		}
	)

	stage = new PrimaryStage {
		title = "TechDraw"
		scene = new Scene(1200, 800) {
			root = new BorderPane {
				top = new ToolBar { content = shapeTools ++ strokeWidthTools ++  colorTools }
				center = drawingPane
			}
		}
	}

	val win = stage.scene.root()

	var click = 0
	win.onMousePressed = { (ev: MouseEvent) =>
		clickedAt = Platform.currentTime
		new Thread {
			override def run {
				val clicked = clickedAt
				Thread.sleep(DOUBLE_CLICK_WAIT_MS)
				if (clicked >= clickedAt) {
					click = ev.clickCount
					if (click >= 2) PolylineControl.reset // ダブルクリックした際に Polyline を終了させる
				}
			}
		}.start
	}

	win.onKeyPressed = { (ev: KeyEvent) =>
		println(f"Key ${ev.code.name} was pressed")
		keyevent(ev)
	}
	win.requestFocus()
}