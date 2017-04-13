import React, { Component } from 'react';

import { Modal } from 'react-bootstrap';
import { Button, Glyphicon } from 'react-bootstrap';
import { InputGroup, ControlLabel } from 'react-bootstrap';
import { Form, FormGroup, Col, FormControl } from 'react-bootstrap';

import Typeahead from 'react-bootstrap-typeahead';
import 'react-bootstrap-typeahead/css/Typeahead.css';

// TODO:
//   - typeahead + addon + onClick
//   - vip label
//      <span class="field-vip-label label label-danger" data-bind="visible: phoneVip">
//        <i class="glyphicon glyphicon-star"></i>VIP</span>
//   - fix tests
//   - report send errors
//   - add $program_info$ keys

const propTypes = {
  isVisible: React.PropTypes.bool.isRequired,
  onHide: React.PropTypes.func.isRequired,
  smsTemplates: React.PropTypes.array,
  values: React.PropTypes.object
};

const defaultProps = {
  smsTemplates: [],
  values: { phone: '' }
};


export default class SmsForm extends Component {

  constructor(props) {
    super(props);
    this.state = {
      phone: this.props.values.phone || '',
      selectedTemplate: [],
      msgText: ''
    };
  }


  _templateChange = (tpl) => {
    this.setState({
      selectedTemplate: tpl,
      msgText: tpl.length > 0
        ? this._renderTemplate(tpl[0].text)
        : this.state.msgText
    });
  }

  _renderTemplate = msg => {
    for(const k in this.props.values) {
      const rx = new RegExp(`\\$${k}\\$`, 'g');
      msg = msg.replace(rx, this.props.values[k]);
    }
    return msg;
  }


  _send = () =>
    fetch('/_/Sms',
      { method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        data: JSON.stringify({
          status: 'please-send',
          caseRef: this.props.values['case.id'],
          phone: this.state.phone,
          template: this.state.selectedTemplate.length > 0
            ? this.state.selectedTemplate[0].id
            : null,
          msgText: this.state.msgText
        })
      })
    .then(resp => {
      if(resp.status === 200) {
        this.setState(
          { selectedTemplate: [], msgText: '' },
          this.props.onHide)
      }
    })


  render() {
    const formGroup = (label, valid, control) => {
      return (
        <FormGroup validationState={valid || 'error'}>
          <Col sm={4} componentClass={ControlLabel}>{label}</Col>
          <Col sm={8}>{control}</Col>
        </FormGroup>
      );
    };

    // TODO: get phone regexp from model
    const phoneIsValid = this.state.phone.match(/^\+\d{11}$/);
    const textIsValid = !!this.state.msgText;
    const canSend = phoneIsValid && textIsValid;


    return (
      <Modal show={this.props.isVisible} onHide={this.props.onHide}>
        <Modal.Header closeButton>
          <Modal.Title>Отправка СМС</Modal.Title>
        </Modal.Header>
        <Modal.Body>
          <Form horizontal>
            {formGroup('Номер кейса', true,
              <FormControl disabled type="number"
                value={this.props.values['case.id']}
              />)
            }
            {formGroup('Телефон получателя', phoneIsValid,
              <InputGroup bsSize="sm">
                <FormControl type="tel"
                  value={this.state.phone}
                  onChange={(e) => this.setState({phone: e.target.value})}
                />
                <InputGroup.Addon>
                  <Glyphicon bsClass="stolen-icon" glyph="phone" />
                </InputGroup.Addon>
              </InputGroup>)
            }
            {formGroup('Шаблон сообщения', true,
              <Typeahead emptyLabel="Ничего не найдено"
                options={this.props.smsTemplates}
                selected={this.state.selectedTemplate}
                onChange={this._templateChange}
              />)
            }
            {formGroup('Текст сообщения', textIsValid,
              <textarea className="form-control" rows="7"
                value={this.state.msgText}
                onChange={(e) => this.setState({msgText: e.target.value})}>
              </textarea>)
            }
          </Form>
        </Modal.Body>
        <Modal.Footer>
          <Button bsStyle="primary" onClick={this.props.onHide}>
            Отмена
          </Button>
          <Button bsStyle="success" onClick={this._send} disabled={!canSend}>
            Отправить
          </Button>
        </Modal.Footer>
      </Modal>
    );
  }
}

SmsForm.propTypes = propTypes;
SmsForm.defaultProps = defaultProps;
