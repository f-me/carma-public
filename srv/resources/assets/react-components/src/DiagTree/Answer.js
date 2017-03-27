import Immutable from 'immutable'
import React from 'react'
import { Grid, Row, Col } from 'react-bootstrap'
import { ListGroupItem, OverlayTrigger, Tooltip } from 'react-bootstrap'
import { Button, ButtonToolbar, Glyphicon } from 'react-bootstrap'

import AnswerEditor from './AnswerEditor'


export default class Answer extends React.Component {

  constructor(props) {
    super(props);
    this.state = this._defaultState(props);
  }

  componentWillReceiveProps(props) {
    this.setState(this._defaultState(props));
  }


  _defaultState = (props) => ({
    answer: props.answer || this._defaultAnswer,
    edit: !props.answer,
    hover: false
  });


  _defaultAnswer = Immutable.Map({
    header: '',
    text: '',
    action: {}
  })


  _cancel = () => {
    if (this.props.answer === null) {
      this.props.onDelete()
    } else {
      this.setState({
        edit: false,
        answer: this.props.answer
      });
    }
  }


  render() {
    const {answer, edit, hover} = this.state;
    const tooltip = text => (<Tooltip>{text}</Tooltip>);
    if (!edit) {
      return (
        <Grid
          className="Answer"
          onMouseEnter={() => this.setState({hover: true})}
          onMouseLeave={() => this.setState({hover: false})}>
          <Row>
            <Col md={7}>
              <ListGroupItem header={answer.get('header')}>
                {answer.get('file') && <img src={answer.get('file')} role="presentation"/>}
                {answer.get('text')}
              </ListGroupItem>
            </Col>
            <Col md={2}>
              { hover &&
                <ButtonToolbar>
                  <OverlayTrigger placement="top" overlay={tooltip('Редактировать')}>
                    <Glyphicon
                      className="btn"
                      onClick={() => this.setState({edit: true})}
                      glyph="pencil"/>
                  </OverlayTrigger>
                  <OverlayTrigger placement="top" overlay={tooltip('Удалить')}>
                    <Glyphicon
                      className="btn"
                      onClick={this.props.onDelete}
                      glyph="trash"/>
                  </OverlayTrigger>
                </ButtonToolbar>
              }
            </Col>
          </Row>
        </Grid>
      );
    } else {
      return (
        <AnswerEditor answer={answer}
          onSave={this.props.onChange}
          onCancel={this._cancel} />
      );
    }
  }
}
