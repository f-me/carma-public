import React, { Component } from 'react';
import { Modal, Button } from 'react-bootstrap';


export default class SmsForm extends Component {

  constructor(props) {
    super(props);
    this.state = { isVisible: false };
  }

  canSend() {
    return false;
  }

  send() {
    console.log('Sending SMS');
  }

  render() {
    return (
      <Modal show={this.props.isVisible} onHide={this.props.onHide}>
        <Modal.Header closeButton>
          <Modal.Title>Отправка СМС</Modal.Title>
        </Modal.Header>
        <Modal.Body>
            <h4>Text in a modal</h4>
            <hr />
        </Modal.Body>
        <Modal.Footer>
          <Button bsStyle="primary" onClick={this.props.onHide}>
            Отмена
          </Button>
          <Button bsStyle="success" onClick={this.send} disabled={!this.canSend()}>
            Отправить
          </Button>
        </Modal.Footer>
      </Modal>
    );
  }
}
