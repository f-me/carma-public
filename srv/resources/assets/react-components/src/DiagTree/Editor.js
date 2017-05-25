import React from 'react'
import { Grid, Row, Col } from 'react-bootstrap'
import { Button, ButtonToolbar, Glyphicon } from 'react-bootstrap'
import Immutable from 'immutable'

import Tree from './Tree'
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
  }

  componentDidMount() {
    this._loadSlides()
  }


  _loadSlides = () => {
    if (this.props.testMode) {
//      const slides_json = require('./slides.json');
      this.setState({
        slides: Immutable.Map(
          slides_json.reduce((m, s) => {m[String(s.id)] = s; return m;}, {})),
          selectedId: this.state.selectedId || slides_json.find(x => x.isRoot).id
      })
    } else {
      fetch('/_/DiagSlide', {credentials: 'same-origin'})
        .then(resp => resp.json().then(slides => {
          slides = slides.filter(s => s.isActive);
          this.setState({
            slides: Immutable.Map(
              slides.reduce((m, s) => {m[String(s.id)] = s; return m;}, {})),
            selectedId: this.state.selectedId || slides.find(x => x.isRoot).id
          });
        }))
    }
  }


  newSlide = () => {
    const slide = {
      header: 'Новый вопрос',
      body: '?',
      resources: [],
      answers: [],
      actions: [],
      isRoot: true
    };

    fetch('/_/DiagSlide',
      { method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        credentials: 'same-origin',
        body: JSON.stringify(slide),
      })
      .then(resp => resp.json().then(res => {
        Object.assign(slide, res);
        this.setState({
          slides: this.state.slides.set(String(slide.id), slide),
          selectedId: slide.id
        })
      }))
  }


  saveSlide = data => {
    let slide = data.toJS();
    this.setState({saveMsg: "Сохраняем"});

    fetch(`/_/DiagSlide/${slide.id}`,
      { method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        credentials: 'same-origin',
        body: JSON.stringify(slide),
      })
      .then(resp => {
        if(resp.status === 200) {
          resp.json().then(res => {
            slide = Object.assign(slide, res);
            this.setState({
              slides: this.state.slides.set(String(slide.id), slide),
              saveMsg: "Сохранено"
            })
            this._loadSlides();
            setTimeout(() => this.setState({saveMsg: ''}), 2000);
          })
        } else {
          this.setState({saveMsg: "Ошибка"})
        }
      })
  }

  deleteSlide = id => {
    fetch(`/_/DiagSlide/${id}`,
      { method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        credentials: 'same-origin',
        body: JSON.stringify({isActive: false}),
      })
      .then(resp => { if(resp.status === 200) this._loadSlides(); })
  }



  render() {
    const {slides, selectedId} = this.state;

    if (slides === null) return (<span>Loading...</span>);

    return (
      <Grid className="Editor">
        <Row>
          <Col md={4}>
            <ButtonToolbar style={{padding: '10px'}}>
              <Button bsStyle='success' onClick={this.newSlide}>
                <Glyphicon glyph="plus" /> Новое дерево
              </Button>
            </ButtonToolbar>
            <Tree
              items={slides}
              selected={selectedId}
              onSelect={it => this.setState({selectedId: it.id})}
            />
          </Col>
          <Col md={8}>
            <SlideEditor
              slide={slides.get(String(selectedId))}
              onChange={this.saveSlide}
              onDelete={this.deleteSlide}
              saveMsg={this.state.saveMsg}
            />
          </Col>
        </Row>
      </Grid>
    );
  }
}
