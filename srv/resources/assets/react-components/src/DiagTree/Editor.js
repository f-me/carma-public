
import React from 'react'
import { Grid, Row, Col } from 'react-bootstrap'
import { Button, ButtonToolbar, Glyphicon } from 'react-bootstrap'
import { TreeList }  from 'react-treeview-mui'

import injectTapEventPlugin from 'react-tap-event-plugin'
import Immutable from 'immutable'

import SlideEditor from './SlideEditor'
import './DiagTree.css'


injectTapEventPlugin();

// FIXME: error if there is no slides
export default class Editor extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      slides: null,
      selectedIx: 0,
      expandedListItems: [],
    }

    this._loadSlides()
  }


  _loadSlides = () =>
    $.ajax({
      type: 'GET',
      url: '/_/DiagSlide',
      dataType: 'json',
      success: slides => {
        const slidesMap = Immutable.Map(
          slides.reduce((m, s) => {m[String(s.id)] = s; return m;}, {}));
        const listItems = this._prepareListItems(slidesMap);
        this.setState({
          slides: slidesMap,
          listItems,
          selectedIx: this.state.selectedIx || 1
        })
      }
    })

  _prepareListItems = slides => {
    const root = [{depth:0, children: []}];
    const listItems = root.concat(slides.valueSeq().toArray());

    const id2ix = {};
    listItems.forEach((it, i) => {id2ix[it.id] = i});

    listItems.forEach((it, i) => {
      if (i > 0) {
        it.ix = i;
        it.children = it.answers.map(z => id2ix[z.nextSlide]);
        it.children.forEach(c => listItems[c].parentIndex = i);
        if(it.isRoot) {
          it.depth = 1;
          it.parentIndex = 0;
        }
      }
    })

    listItems.forEach((it, i) => {
      it.depth = it.parentIndex !== undefined
        ? listItems[it.parentIndex].depth + 1
        : 0;
    })

    return listItems;
  }


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
          selectedId: slide.id,
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


  _handleTreeTap = (item, index) => {
    const {expandedListItems} = this.state;
    if (expandedListItems.indexOf(index) === -1) {
      this.setState({
        expandedListItems: [index].concat(expandedListItems),
        selectedIx: index
      })
    } else {
      this.setState({
        expandedListItems: expandedListItems.filter(x => x != index),
        selectedIx: index
      })
    }
  }


  render() {
    const {slides, listItems, expandedListItems, selectedIx} = this.state;

    if (slides === null) return (<span>Loading...</span>);

    return (
      <Grid className="Editor">
        <Row>
          <Col md={4}>
            <TreeList
              listItems={listItems}
              contentKey={'header'}
              haveSearchbar={true}
              handleTouchTap={this._handleTreeTap}
              expandedListItems={expandedListItems}
              activeListItem={selectedIx}
              >
                <ButtonToolbar>
                  <Button bsStyle='success' onClick={this.newSlide}>
                    <Glyphicon glyph="plus" /> Новое дерево
                  </Button>
                </ButtonToolbar>
            </TreeList>
          </Col>
          <Col md={8}>
            <SlideEditor
              slide={slides.get(String(listItems[selectedIx].id))}
              onChange={this.saveSlide}
              saveMsg={this.state.saveMsg}
            />
          </Col>
        </Row>
      </Grid>
    );
  }
}
