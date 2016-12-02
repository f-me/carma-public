import Immutable from 'immutable'
import React from 'react'
import { Grid, Row, Col } from 'react-bootstrap'

import SlideTree from './SlideTree'
import SlideEditor from './SlideEditor'
import './Editor.css'



export default class Editor extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      slides: Immutable.fromJS([]),
      currentSlide: null
    }

    this.componentWillReceiveProps(props);
  }


  componentWillReceiveProps(props) {
    this.loadSlides(slides => this.setState({
      slides: Immutable.fromJS(slides),
      currentSlide: Immutable.fromJS(slides.find(x => x.isRoot))
    }));
  }

  shouldComponentUpdate(nextProps, {slides, currentSlide}) {
    return this.state.currentSlide !== currentSlide
        || this.state.slides !== slides
  }


  loadSlides = callback =>
    $.ajax({
      type: 'GET',
      url: '/_/DiagSlide',
      dataType: 'json',
      success: callback
    })


  newSlide = () => {
    let slide = {
      header: 'Новый вопрос',
      body: '?',
      resources: [],
      answers: [],
      isRoot: true
    };

    $.ajax({
      type: 'POST',
      url: '/_/DiagSlide',
      data: JSON.stringify(slide),
      processData: false,
      contentType: 'application/json',
      success: res => {
        slide.id = res.id;
        slide = Immutable.fromJS(slide);
        this.setState({
          slides: this.state.slides.push(slide),
          currentSlide: slide
        })
      }
    })
  }


  // FIXME: add new slides for answers
  saveSlide = data => {
    const {slides, currentSlide} = this.state;
    const id = currentSlide.get("id");
    const ix = slides.findIndex(x => x.get("id") == id);

    $.ajax({
      type: 'PUT',
      url: `/_/DiagSlide/${id}`,
      data: JSON.stringify(data.toJS()),
      processData: false,
      contentType: 'application/json',
      success: () => this.setState({
        slides: slides.set(ix, data),
        currentSlide: data
      })
    });
  }


  render() {
    const {slides, currentSlide} = this.state;
    console.log('Editor.render', currentSlide && currentSlide.toJS());

    return (
      <Grid className="Editor">
        <Row>
          <Col md={4}>
            <SlideTree
              slides={slides}
              currentSlide={currentSlide}
              onSelect={s => this.setState({currentSlide: s})}
              onAddSlide={this.newSlide}
            />
          </Col>
          <Col md={8}>
            { currentSlide &&
              <SlideEditor
                slide={currentSlide}
                onChange={this.saveSlide}
              />
            }
          </Col>
        </Row>
      </Grid>
    );
  }
}
