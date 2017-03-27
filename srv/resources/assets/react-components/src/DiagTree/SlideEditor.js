import Immutable from 'immutable'
import React from 'react'
import { FormGroup, FormControl, ControlLabel } from 'react-bootstrap'
import { ButtonToolbar, Button, Glyphicon } from 'react-bootstrap'
import { ListGroup, ListGroupItem } from 'react-bootstrap'

import RichTextEditor from 'react-rte'
import Typeahead from 'react-bootstrap-typeahead';
import 'react-bootstrap-typeahead/css/Typeahead.css';

import Resource from './Resource'
import Answer from './Answer'


export default class SlideEditor extends React.Component {

  constructor(props) {
    super(props);
    this.state = this._defaultState(props);
  }

  componentWillReceiveProps(props) {
    this.setState(this._defaultState(props));
  }

  _defaultState = props => ({
    slide: Immutable.fromJS(props.slide),
    rtValue: RichTextEditor.createValueFromString(props.slide.body, 'markdown'),
    draftResource: false
  })


  _revert = () => this.setState(this._defaultState(this.props))

  _setResource = (i, resource) => this.setState(st => ({
    draftResource: false,
    slide: st.slide.update('resources',
        as => i === null ? as.push(resource) : as.set(i, resource)
    )
  }))

  _defaultResource = Immutable.Map({
    files: [],
    text: ''
  })


  _rtChanged = value => this.setState({
    rtValue: value,
    slide: this.state.slide.set('body', value.toString('markdown'))
  })

  _addAction = actions => {
    this.setState({
      slide: this.state.slide.set('actions', Immutable.fromJS(actions))
    });
  }


  render() {
    const {slide} = this.state;

    const isNotChanged = slide.equals(this.props.slide);
    const setSlide = (f, v) => this.setState({slide: slide.set(f, v)})

    const resources = slide.get('resources').toArray().map((res, i) => (
      <Resource key={i} resource={res}
        onDelete={() => this.setState({slide: slide.deleteIn(['resources', i])})}
        onChange={x  => this.setState({slide: slide.setIn(['resources', i], x)})}
      />
    )).concat(
      <ListGroupItem key={-1}>
        <Button
          onClick={() => this.setState({
              slide: slide.updateIn(['resources'], as => as.push(null))
          })}
        >
          <Glyphicon glyph="plus" /> Добавить картинку
        </Button>
      </ListGroupItem>
    );

    const answers = slide.get('answers').toArray().map((ans,i) => (
      <Answer key={i} answer={ans}
        onDelete={() => this.setState({slide: slide.deleteIn(['answers', i])})}
        onChange={x  => this.setState({slide: slide.setIn(['answers', i], x)})}
      />
    )).concat(
      <ListGroupItem key={-1}>
        <Button
          onClick={() => this.setState({
              slide: slide.updateIn(['answers'], as => as.push(null))
          })}
        >
          <Glyphicon glyph="plus" /> Добавить ответ
        </Button>
      </ListGroupItem>
    );

    const actions =
      [ {label: "Создать Эвакуацию", svc: "Towage"}
      , {label: "Создать Техпомощь", svc: "Tech"}
      , {label: "Создать Консультацию", svc: "Consultation"}
      ];


    return (
      <form className="SlideEditor">
        <FormGroup>
          <FormControl
            type="text"
            className="header"
            placeholder="Заголовок"
            onChange={e => setSlide("header", e.target.value)}
            value={slide.get("header")} />
        </FormGroup>

        <FormGroup>
          <RichTextEditor
            placeholder="Описание"
            onChange={this._rtChanged}
            value={this.state.rtValue} />
        </FormGroup>

        <FormGroup>
          <ControlLabel>Картинки</ControlLabel>
          <ListGroup>{resources}</ListGroup>
        </FormGroup>

        { slide.get('actions').toArray().length === 0 &&
          <FormGroup>
            <ControlLabel>Ответы</ControlLabel>
            <ListGroup>{answers}</ListGroup>
          </FormGroup>
        }
        { slide.get('answers').toArray().length === 0 &&
          <FormGroup>
            <ControlLabel>Рекомендация</ControlLabel>
            <Typeahead emptyLabel="Что делать?"
              options={actions}
              selected={slide.get('actions').toJS()}
              onChange={this._addAction}
            />
          </FormGroup>
        }

        <ButtonToolbar>
          <Button
            disabled={isNotChanged}
            onClick={this._revert}>Отменить изменения</Button>
          <Button
            disabled={isNotChanged}
            bsStyle="success"
            onClick={() => this.props.onChange(slide)}>Сохранить</Button>
          <span style={{margin: "10px 5px"}}>
            {this.props.saveMsg}
          </span>
        </ButtonToolbar>
      </form>
    );
  }
}
