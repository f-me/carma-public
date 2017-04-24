import Immutable from 'immutable'
import React from 'react'
import { Grid, Row, Col } from 'react-bootstrap'
import { OverlayTrigger, Tooltip } from 'react-bootstrap'
import { ButtonToolbar, Glyphicon } from 'react-bootstrap'

import ResourceEditor from './ResourceEditor'


export default class Resource extends React.Component {

  constructor(props) {
    super(props);
    this.state = this._defaultState(props);
  }

  componentWillReceiveProps(props) {
    this.setState(this._defaultState(props));
  }


  _defaultState = (props) => ({
    resource: props.resource || this._defaultResource,
    edit: !props.resource,
    hover: false
  });


  _defaultResource = Immutable.Map({
    file: null,
    text: '',
  })


  _cancel = () => {
    if (this.props.resource === null) {
      this.props.onDelete()
    } else {
      this.setState({
        edit: false,
        resource: this.props.resource
      });
    }
  }


  render() {
    const {resource, edit, hover} = this.state;
    const tooltip = text => (<Tooltip>{text}</Tooltip>);

    if (!edit) {
      return (
        <Grid
          onMouseEnter={() => this.setState({hover: true})}
          onMouseLeave={() => this.setState({hover: false})}>
          <Row>
            <Col md={7}>
              <img src={resource.get('file')} role="presentation"/>
              <span>{resource.get('text')}</span>
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
        <ResourceEditor resource={resource}
          onSave={this.props.onChange}
          onCancel={this._cancel} />
      );
    }
  }
}

