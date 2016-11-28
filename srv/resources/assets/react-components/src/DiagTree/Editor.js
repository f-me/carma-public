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
      slides: [],
      currentSlide: null
    }

    this.componentWillReceiveProps(props);
  }


  componentWillReceiveProps(props) {
    this.loadSlides(slides => this.setState({
      slides,
      currentSlide: Immutable.fromJS(slides.find(x => x.isRoot))
    }));
  }


  loadSlides(callback) {
    $.ajax({
      type: 'GET',
      url: '/_/DiagSlide',
      dataType: 'json',
      success: callback
    });
  }


  newSlide() {
  }


  saveSlide() {
  }


  render() {
    const {slides, currentSlide } = this.state;

    return (
      <Grid className="Editor">
        <Row>
          <Col md={4}>
            <SlideTree
              slides={slides}
              currentSlide={currentSlide}
              onSelect={id => this.setState({currentSlide: id})}
              onAddSlide={() => this.newSlide()}
            />
          </Col>
          <Col md={8}>
            { currentSlide &&
              <SlideEditor
                slide={currentSlide}
                onChange={data => this.saveSlide(currentSlide, data)}
              />
            }
          </Col>
        </Row>
      </Grid>
    );
  }
}
