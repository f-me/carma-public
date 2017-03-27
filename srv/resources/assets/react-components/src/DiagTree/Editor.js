import Immutable from 'immutable'
import React from 'react'
import { Grid, Row, Col } from 'react-bootstrap'

import SlideTree from './SlideTree'
import SlideEditor from './SlideEditor'
import './DiagTree.css'



// FIXME: error if there is no slides
export default class Editor extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      slides: null,
      selectedId: null
    }

    this._loadSlides()
  }


  _loadSlides = () =>
    $.ajax({
      type: 'GET',
      url: '/_/DiagSlide',
      dataType: 'json',
      success: slides => this.setState({
        slides: Immutable.Map(
          slides.reduce((m, s) => {m[String(s.id)] = s; return m;}, {})),
        selectedId: this.state.selectedId || slides.find(x => x.isRoot).id
      })
    })


  newSlide = () => {
    const slide = {
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
        Object.assign(slide, res);
        this.setState({
          slides: this.state.slides.set(String(slide.id), slide),
          selectedId: slide.id
        })
      }
    })
  }


  saveSlide = data => {
    let slide = data.toJS();
    this.setState({saveMsg: "Сохраняем"});

    $.ajax({
      type: 'PUT',
      url: `/_/DiagSlide/${slide.id}`,
      data: JSON.stringify(slide),
      processData: false,
      contentType: 'application/json',
      success: res => {
        slide = Object.assign(slide, res);
        this.setState({
          slides: this.state.slides.set(String(slide.id), slide),
          saveMsg: "Сохранено"
        })
        this._loadSlides();
        setTimeout(() => this.setState({saveMsg: ''}), 2000);
      },
      error: () => this.setState({saveMsg: "Ошибка"})
    });
  }


  render() {
    const {slides, selectedId} = this.state;
    if (selectedId === null) return (<span>Loading...</span>);

    return (
      <Grid className="Editor">
        <Row>
          <Col md={4}>
            <SlideTree
              slides={slides}
              selectedId={selectedId}
              onSelect={s => this.setState({selectedId: s})}
              onAddSlide={this.newSlide}
            />
          </Col>
          <Col md={8}>
            <SlideEditor
              slide={slides.get(String(selectedId))}
              onChange={this.saveSlide}
              saveMsg={this.state.saveMsg}
            />
          </Col>
        </Row>
      </Grid>
    );
  }
}
