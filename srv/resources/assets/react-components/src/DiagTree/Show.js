import React from 'react'
import { Grid, Row, Col } from 'react-bootstrap'
import { ListGroup, ListGroupItem } from 'react-bootstrap'
import RichTextEditor from 'react-rte'

import './DiagTree.css';


//FIXME: - caseId to props?
//       - immutable history
export default class Show extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      history: null,
      path: [0]
    };

    this._loadHistory();
    this._loadSlides();
  }


  _loadSlides = () =>
    $.ajax({
      type: 'GET',
      url: '/_/DiagSlide',
      dataType: 'json',
      success: slides => this.setState({slides})
    })


  _loadHistory = () => {
    $.ajax({
      type: 'GET',
      url: `/diag/history/${this.props.caseId}`,
      dataType: 'json',
      success: hist => this.setState({history: hist, path: [hist.length-1]})
    })
  }

  _answer = ix => () => {
    const {history, path} = this.state;
    const slide = history[path[0]]; // FIXME:
    $.ajax({
      type: 'PUT',
      url: `/_/DiagHistory/${slide.id}`,
      data: JSON.stringify({answerIx: ix}),
      processData: false,
      contentType: 'application/json',
      success: () =>
        $.ajax({
          type: 'POST',
          url: `/_/DiagHistory`,
          data: JSON.stringify({
            caseId: Number.parseInt(this.props.caseId),
            // FIXME: next slide Id
          }),
          processData: false,
          contentType: 'application/json',
          success: newSlide => this.setState({
            // FIXME: join history item & slide
            history: history.concat([newSlide]),
            path: [path[0]+1]
          })
        })
    })
  }


  render() {
    const {history, path} = this.state;
    if (!history) return <span>Loading...</span>;

    const slide = history[path[0]]; // FIXME:
    const body  = RichTextEditor.createValueFromString(slide.body, 'markdown');
    return (
      <Grid className="Show">
        <Row>
          <Col md={4}>
            <ListGroup>
              {history.map((h,i) => (
                <ListGroupItem header={h.header}>
                  {h.answerIx === null ? '...' : h.answers[h.answerIx].header}
                </ListGroupItem>
              ))}
            </ListGroup>
          </Col>
          <Col md={8}>
            <h1>{slide.header}</h1>
            <RichTextEditor readOnly={true} value={body} />
            <ListGroup>
              {slide.answers.map((ans,i) => (
                <ListGroupItem
                  header={ans.header}
                  onClick={slide.answerIx !== null ? undefined : this._answer(i)}
                >
                  {ans.text}
                </ListGroupItem>
              ))}
            </ListGroup>
          </Col>
        </Row>
      </Grid>
    );
  }
}
