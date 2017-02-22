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
    const setResource = (f, v) => this.setState({resource: resource.set(f, v)});
    const setFile = fs => {
      if (fs && fs[0]) {
        const reader = new FileReader();
        reader.onload = () => setResource('file', reader.result);
        reader.readAsDataURL(fs[0]);
      }
    };

    return (
      <div className="ResourceEditor">
        <FormGroup>
          <Dropzone className="dropzone"
            onDrop={setFile}
            accept="audio/*,video/*,image/*">
              Нажмите для добавления картинки или перетащите её сюда
          </Dropzone>
          <img src={resource.get('file')} role="presentation" />
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
