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
    const {answer, edit} = this.state;
    const tooltip = text => (<Tooltip>{text}</Tooltip>);
    if (!edit) {
      return (
        <Grid className="Answer">
          <Row>
            <Col md={7}>
              <ListGroupItem header={answer.get('header')}>
                {answer.get('text')}
              </ListGroupItem>
            </Col>
            <Col md={2}>
              <ButtonToolbar>
                <OverlayTrigger placement="top" overlay={tooltip('Редактировать')}>
                  <Button onClick={() => this.setState({edit: true})}>
                    <Glyphicon glyph="pencil"/>
                  </Button>
                </OverlayTrigger>
                <OverlayTrigger placement="top" overlay={tooltip('Удалить')}>
                  <Button onClick={this.props.onDelete}>
                    <Glyphicon glyph="trash"/>
                  </Button>
                </OverlayTrigger>
              </ButtonToolbar>
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
