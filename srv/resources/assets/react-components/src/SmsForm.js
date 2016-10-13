import React, { Component } from 'react';

import { Modal } from 'react-bootstrap';
import { Button, Glyphicon } from 'react-bootstrap';
import { InputGroup, ControlLabel } from 'react-bootstrap';
import { Form, FormGroup, Col, FormControl } from 'react-bootstrap';

import Typeahead from 'react-bootstrap-typeahead';
import 'react-bootstrap-typeahead/css/Typeahead.css';


const propTypes = {
  isVisible: React.PropTypes.bool.isRequired,
  onHide: React.PropTypes.func.isRequired,
  smsTemplates: React.PropTypes.array,
  defaultValues: React.PropTypes.object
};

const defaultProps = {
  smsTemplates: [],
  defaultValues: { phone: '', caseRef: null }
};


export default class SmsForm extends Component {

  constructor(props) {
    super(props);
    this.state = {
      phone: this.props.defaultValues.phone,
      selectedTemplate: [],
      msgText: ''
    };
  }


  _templateChange = (tpl) => {
    console.log(tpl);
    this.setState({
      selectedTemplate: tpl,
      msgText: tpl.length > 0 ? tpl[0].text : this.state.msgText
    });
  }


  _canSend() {
    return !!this.state.msgText && !!this.state.phone;
  }


  send() {
    this.setState({
      selectedTemplate: [],
      msgText:''
    });
  }


  render() {
    const formGroup = (label, control) => {
      return (
        <FormGroup>
          <Col sm={4} componentClass={ControlLabel}>{label}</Col>
          <Col sm={8}>{control}</Col>
        </FormGroup>
      );
    };

    return (
      <Modal show={this.props.isVisible} onHide={this.props.onHide}>
        <Modal.Header closeButton>
          <Modal.Title>Отправка СМС</Modal.Title>
        </Modal.Header>
        <Modal.Body>
          <Form horizontal>
            {formGroup('Номер кейса',
              <FormControl disabled type="number"
                value={this.props.defaultValues.caseRef}
              />)
            }
            {formGroup('Телефон получателя',
              <InputGroup bsSize="sm">
                <FormControl type="tel"
                  value={this.state.phone}
                  onChange={(e) => this.setState({phone: e.value})}
                />
                <InputGroup.Addon>
                  <Glyphicon bsClass="stolen-icon" glyph="phone" />
                </InputGroup.Addon>
              </InputGroup>)
            }
            {formGroup('Шаблон сообщения',
              <Typeahead emptyLabel="Ничего не найдено"
                options={this.props.smsTemplates}
                selected={this.state.selectedTemplate}
                onChange={this._templateChange}
              />)
            }
            {formGroup('Текст сообщения',
              <textarea className="form-control" rows="7"
                value={this.state.msgText}
                onChange={(e) => this.setState({msgText: e.value})}>
              </textarea>)
            }
          </Form>
        </Modal.Body>
        <Modal.Footer>
          <Button bsStyle="primary" onClick={this.props.onHide}>
            Отмена
          </Button>
          <Button bsStyle="success" onClick={this.send} disabled={!this._canSend()}>
            Отправить
          </Button>
        </Modal.Footer>
      </Modal>
    );
  }
}

SmsForm.propTypes = propTypes;
SmsForm.defaultProps = defaultProps;

// TODO:
//   - add templates to typeahead
//   - include typeahead.css
//   - typeahead + addon
//   - typeahead addon onClick
//   - phone regexp
//   - canSend
//   - render templates
//   - vip label
//   - fix test

/*
  <div class="form-group has-feedback" data-bind="css: { 'has-error': caseRefNot },                                       visible: caseRefVisible                                      ">
    <label class="col-sm-4 control-label"><span data-provide="popover" data-container="body" data-placement="top" data-bind="        text: caseRef.field.meta.label,        attr: {'data-content': caseRef.field.meta.infoText1},        css: {fieldInfo: caseRef.field.meta.infoText1}" class="">Номер кейса</span>
    </label>
    <div class="col-sm-8">
      <div class="input-group input-group-sm">
        <input class="form-control" type="text" autocomplete="off" name="caseRefText" style="" data-bind="
               value: caseRefText,
               valueUpdate: ['afterkeydown', 'afterpaste'],
               readonly: caseRefDisabled,
               "><span class="glyphicon glyphicon-refresh glyphicon-refresh-animate form-control-feedback saving-spinner" data-bind="sync: caseRefSync" style="display: none"></span>
      </div>
    </div>
  </div>

  <div class="form-group has-feedback" data-bind="css: { 'has-error': phoneNot },                                       visible: phoneVisible                                      ">
    <label class="col-sm-4 control-label"><span data-provide="popover" data-container="body" data-placement="top" data-bind="        text: phone.field.meta.label,        attr: {'data-content': phone.field.meta.infoText1},        css: {fieldInfo: phone.field.meta.infoText1}" class="">Телефон получателя</span>
    </label>
    <div class="col-sm-8">
      <div class="input-group input-group-sm">
        <div class="input-group input-group-sm">
          <input class="form-control" type="text" autocomplete="off" name="phone" style="" data-bind="
               value: phone,
               valueUpdate: ['afterkeydown', 'afterpaste'],
               readonly: phoneDisabled,
               css: { 're-failed': phoneRegexp },
               " onkeydown="kdoPick('callPlease', 'phone', 73, event);"><span class="glyphicon glyphicon-refresh glyphicon-refresh-animate form-control-feedback saving-spinner" data-bind="sync: phoneSync" style="display: none"></span><span class="field-vip-label label label-danger" data-bind="visible: phoneVip" style="display: none;"><i class="glyphicon glyphicon-star"></i>VIP</span><span class="input-group-addon"><span class="glyphicon stolen-icon-phone" onclick="doPick('callPlease',                                                    'phone', event.target);"></span></span>
        </div>
      </div>
    </div>
  </div>

  <div class="form-group has-feedback" data-bind="css: { 'has-error': templateNot },                                       visible: templateVisible                                      ">
    <label class="col-sm-4 control-label"><span data-provide="popover" data-container="body" data-placement="top" data-bind="        text: template.field.meta.label,        attr: {'data-content': template.field.meta.infoText1},        css: {fieldInfo: template.field.meta.infoText1}" class="">Шаблон сообщения</span>
    </label>
    <div class="col-sm-8">
      <div class="input-group input-group-sm">
        <input class="form-control " type="text" autocomplete="off" name="template" data-bind="
                value: templateLocal,
                valueUpdate: 'change',
                disabled: templateDisabled,
                pickerDisable: templateDisabled,
                bindDict: 'template',
"><span class="glyphicon glyphicon-refresh glyphicon-refresh-animate form-control-feedback saving-spinner" data-bind="sync: templateSync" style="display: none"></span><span class="input-group-addon"><span class="glyphicon glyphicon-chevron-down"></span></span>
      </div>
    </div>
  </div>

  <div class="form-group has-feedback" data-bind="css: { 'has-error': msgTextNot },                                       visible: msgTextVisible                                      ">
    <label class="col-sm-4 control-label"><span data-provide="popover" data-container="body" data-placement="top" data-bind="        text: msgText.field.meta.label,        attr: {'data-content': msgText.field.meta.infoText1},        css: {fieldInfo: msgText.field.meta.infoText1}" class="">Текст сообщения</span>
    </label>
    <div class="col-sm-8">
      <div class="input-group input-group-sm">
        <textarea class="form-control" name="msgText" rows="7" style="height: 10em" data-bind="
                 value: msgTextText,
                 valueUpdate: ['afterkeydown', 'afterpaste'],
                 disabled: msgTextDisabled,
"></textarea>
      </div>
    </div>
  </div>
</div>
          <div class="modal-footer">
            <button class="btn btn-primary" id="do-send-sms" disabled="disabled">Отправить</button>
          </div>
        </div>
      </div>
    </div>
    */
