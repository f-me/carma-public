import React from 'react'

import { ListGroup, ListGroupItem } from 'react-bootstrap'
import { Button, ButtonToolbar } from 'react-bootstrap'


export default class SlideTree extends React.Component {
  render() {
    return (
      <div>
        <ButtonToolbar>
          <Button bsStyle="success"
            onClick={this.props.onAddSlide}
          >
            + Новое дерево
          </Button>
        </ButtonToolbar>
      </div>
    );
  }
}
