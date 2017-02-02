import Immutable from 'immutable'
import React from 'react'
import { FormGroup, FormControl, ControlLabel } from 'react-bootstrap'
import { ButtonToolbar, Button } from 'react-bootstrap'
import { ListGroup, ListGroupItem } from 'react-bootstrap'

import MdEditor from 'react-md-editor'
import 'react-md-editor/dist/react-md-editor.css'
import 'codemirror/lib/codemirror.css'


import ResourceEditor from './ResourceEditor'
import AnswerEditor from './AnswerEditor'


export default class SlideEditor extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      slide: props.slide,
      draftAnswer: false,
      draftResource: false
    }
  }

  componentWillReceiveProps(props) {
    this.setState({
      slide: props.slide,
      draftAnswer: false,
      draftResource: false
    });
  }


  _revert = () => this.setState({
    slide: this.props.slide,
    draftAnswer: false,
    draftResource: false
  })


  _setAnswer = (i, answer) => this.setState(st => ({
    draftAnswer: false,
    slide: st.slide.update('answers',
        as => i === null ? as.push(answer) : as.set(i, answer)
    )
  }))

  _defaultAnswer = Immutable.Map({
    header: '',
    text: '',
    action: {}
  })

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


  render() {
    const {slide} = this.state;
    console.log('SlideEditor.render', slide.toJS());

    const isNotChanged = slide.equals(this.props.slide);
    const setSlide = (f, v) => this.setState({slide: slide.set(f, v)})

    const resources = slide.get('resources').toArray().map((res, i) => (
      <div key={i}>
        <img src={res.get('files')[0].preview} role="presentation" />
        <span>{res.get('text')}</span>
      </div>
    )).concat(
      <ListGroupItem key={-1}>
        { this.state.draftResource
            ? <ResourceEditor resource={this._defaultResource}
                onSave={res => this._setResource(null, res)}
                onCancel={() => this.setState({draftResource: false})} />
            : <Button onClick={() => this.setState({draftResource: true})}>
                + Добавить картинку
              </Button>
        }
      </ListGroupItem>
    );

    const answers = slide.get('answers').toArray().map((ans,i) => (
      <ListGroupItem key={i} header={ans.get('header')}>
        {ans.get('text')}
      </ListGroupItem>
    )).concat(
      <ListGroupItem key={-1}>
        { this.state.draftAnswer
            ? <AnswerEditor answer={this._defaultAnswer}
                onSave={ans => this._setAnswer(null, ans)}
                onCancel={() => this.setState({draftAnswer: false})} />
            : <Button
                onClick={() => this.setState({draftAnswer: true})}>
                  + Добавить ответ
              </Button>
        }
      </ListGroupItem>
    );


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
          <MdEditor
            placeholder="Описание"
            rows={8}
            onChange={txt => setSlide("body", txt)}
            value={slide.get("body")} />
        </FormGroup>

        <FormGroup>
          <ControlLabel>Картинки</ControlLabel>
          <ListGroup>{resources}</ListGroup>
        </FormGroup>

        <FormGroup>
          <ControlLabel>Ответы</ControlLabel>
          <ListGroup>{answers}</ListGroup>
        </FormGroup>

        <ButtonToolbar>
          <Button
            disabled={isNotChanged}
            onClick={this._revert}>Отменить изменения</Button>
          <Button
            disabled={isNotChanged}
            bsStyle="success"
            onClick={() => this.props.onChange(slide)}>Сохранить</Button>
        </ButtonToolbar>
      </form>
    );
  }
}
