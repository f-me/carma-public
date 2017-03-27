import React from 'react';
import { FormGroup, FormControl } from 'react-bootstrap'
import { Button, ButtonToolbar } from 'react-bootstrap'
import Dropzone from 'react-dropzone'


export default class AnswerEditor extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      answer: this.props.answer
    };
  }

  render() {
    const {answer} = this.state;
    const isNotChanged = answer.equals(this.props.answer);
    const setAnswer = (f, v) => this.setState({answer: answer.set(f, v)});
    const setFile = fs => {
      if (fs && fs[0]) {
        const reader = new FileReader();
        reader.onload = () => setAnswer('file', reader.result);
        reader.readAsDataURL(fs[0]);
      }
    };

    return (
      <div>
        <FormGroup>
          <FormControl type="text" placeholder="Ответ"
            value={answer.get('header')}
            onChange={e => setAnswer('header', e.target.value)}
          />
        </FormGroup>

        <FormGroup>
          <FormControl type="text" placeholder="Комментарий"
            value={answer.get('text')}
            onChange={e => setAnswer('text', e.target.value)}
          />
        </FormGroup>

        <FormGroup>
          <Dropzone className="dropzone"
            onDrop={setFile}
            accept="audio/*,video/*,image/*">
              Нажмите для добавления картинки или перетащите её сюда
          </Dropzone>
          <img src={answer.get('file')} role="presentation" />
        </FormGroup>

        <ButtonToolbar>
          <Button
            onClick={() => this.props.onCancel()}>Отменить</Button>
          <Button
            disabled={isNotChanged}
            bsStyle="success"
            onClick={() => this.props.onSave(answer)}>
              Сохранить ответ
          </Button>
        </ButtonToolbar>
      </div>
    );
  }
}
