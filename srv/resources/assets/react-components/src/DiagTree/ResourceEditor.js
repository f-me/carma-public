import React from 'react'
import { FormGroup, FormControl } from 'react-bootstrap'
import { ButtonToolbar, Button } from 'react-bootstrap'
import Dropzone from 'react-dropzone'


export default class ResourceEditor extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      resource: this.props.resource,
    };
  }


  render() {
    const {resource} = this.state;
    const isNotChanged = resource.equals(this.props.resource);
    const setResource = (f, v) => this.setState({resource: resource.set(f, v)})

    return (
      <div className="ResourceEditor">
        <FormGroup>
          <Dropzone className="dropzone"
            onDrop={fs => setResource('files', fs)}
            accept="audio/*,video/*,image/*">
              Нажмите для добавления картинки или перетащите её сюда
          </Dropzone>
          { resource.get('files').length
              ?  <img src={resource.get('files')[0].preview} role="presentation" />
              : null
          }
          <FormControl type="text"
            placeholder="Подпись к картинке"
            onChange={e => setResource('text', e.target.value)}
            />
        </FormGroup>

        <ButtonToolbar>
          <Button
            onClick={() => this.props.onCancel()}>Отменить</Button>
          <Button
            disabled={isNotChanged}
            bsStyle="success"
            onClick={() => this.props.onSave(resource)}>
              Сохранить
          </Button>
        </ButtonToolbar>
      </div>

    );
  }
}
