import React from 'react'
import { Grid, Row, Col } from 'react-bootstrap'
import { ListGroup, ListGroupItem } from 'react-bootstrap'
import { OverlayTrigger, Tooltip } from 'react-bootstrap'
import { Button, Glyphicon } from 'react-bootstrap'
import RichTextEditor from 'react-rte'

import './DiagTree.css';


// FIXME: - immutable history?
export default class Show extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      history: null,
      slideId: null,
      hoverId: null,
      showDeprecated: null
    };
    this._loadHistory();
  }


  _loadHistory = () => {
    $.ajax({
      type: 'GET',
      url: `/diag/history/${this.props.caseId}`,
      dataType: 'json',
      success: hist => this.setState({
        history: hist,
        slideId: hist[hist.length-1].id
      })
    })
  }

  _answer = (slideId, ix, nextSlide) => () => {
    $.ajax({
      type: 'PUT',
      url: `/_/DiagHistory/${slideId}`,
      data: JSON.stringify({answerIx: ix}),
      processData: false,
      contentType: 'application/json',
      success: () =>
        $.ajax({
          type: 'POST',
          url: `/_/DiagHistory`,
          data: JSON.stringify({
            caseId: Number.parseInt(this.props.caseId),
            slideId: nextSlide
          }),
          processData: false,
          contentType: 'application/json',
          success: this._loadHistory
        })
    })
  }


  _repeatQuestion = histId => () => {
    $.ajax({
      type: 'POST',
      url: `/diag/retry/${histId}`,
      data: "{}",
      processData: false,
      contentType: 'application/json',
      success: this._loadHistory
    })
  }

  _execAction = act => () => {
    $.ajax({
      type: 'POST',
      url: `/_/${act.svc}`,
      data: JSON.stringify({
        parentId: Number.parseInt(this.props.caseId)
      }),
      processData: false,
      contentType: 'application/json',
      success: alert('Готово')
    })
  }


  render() {
    const {history, slideId, hoverId} = this.state;
    if (!history) return <span>Loading...</span>;

    const slide = history.find(h => h.id === slideId);
    const body  = RichTextEditor.createValueFromString(slide.body, 'markdown');
    const answer = (hist, opt={}) => {
      if(hist.answerIx === null) return '';
      return (<div>
        <div className="history-answer">
          {hist.answers[hist.answerIx].header}
          <br/>
          {hist.answerTime} − {hist.answeredBy}
        </div>
        { !opt.disabled && hoverId ===  hist.id
          ?  <OverlayTrigger
                placement="top"
                overlay={<Tooltip>Повторить вопрос</Tooltip>}>
              <Glyphicon
                  className="btn floating-btn"
                  onClick={this._repeatQuestion(hist.id)}
                  glyph="repeat"/>
            </OverlayTrigger>
          : ''
        }
      </div>);
    };

    return (
      <Grid className="Show">
        <Row>
          <Col md={4}>
            <ListGroup>
              {history.filter(h => h.deprecatedBy === null).map((h,i) => {
                const prevHistory = history.filter(hh => hh.deprecatedBy === h.id);
                return (
                  <div>
                    <div>
                      { prevHistory.length && this.state.showDeprecated != h.id
                        ? <a className="more" href="#"
                            onClick={() => this.setState({showDeprecated: h.id})}
                          >
                            Показать отменённые ответы
                          </a>
                        : ''
                      }
                      { this.state.showDeprecated == h.id
                        ? <a className="more" href="#"
                            onClick={() => this.setState({showDeprecated: null})}
                          >
                            Скрыть отменённые ответы
                          </a>
                        : ''
                      }
                    </div>
                    { prevHistory.length && this.state.showDeprecated == h.id
                      ? <ListGroup>
                        { history.filter(hh => hh.deprecatedBy === h.id).map((hh, j) =>
                            <ListGroupItem key={j}
                              className={hh.id === slideId ? 'deprecated selected' : 'deprecated'}
                              header={hh.header}
                              onClick={() => this.setState({slideId: hh.id})}
                            >
                              {answer(h, {disabled: true})}
                            </ListGroupItem>
                        )}
                        </ListGroup>
                      : ''
                    }
                    <ListGroupItem
                      className={h.id === slideId ? 'selected' : ''}
                      onClick={() => this.setState({slideId: h.id})}
                      onMouseEnter={() => this.setState({hoverId: h.id})}
                      onMouseLeave={() => this.setState({hoverId: null})}
                      header={h.header}
                    >
                      {answer(h)}
                    </ListGroupItem>
                  </div>);
              })}
            </ListGroup>
          </Col>
          <Col md={8}>
            <h1>{slide.header}</h1>
            <RichTextEditor readOnly={true} value={body} />
            {slide.resources.map((res, i) => (
              <div key={i}>
                <img src={res.file} role="presentation"/>
                <span>{res.text}</span>
              </div>
            ))}
            <ListGroup>
              {slide.answers.map((ans, i) => (
                <ListGroupItem
                  header={ans.header}
                  className={slide.answerIx === i ? 'selected' : ''}
                  onClick={slide.answerIx !== null
                    ? undefined
                    : this._answer(slide.id, i, ans.nextSlide)}
                >
                  {ans.file && <img src={ans.file} role="presentation"/>}
                  {ans.text}
                </ListGroupItem>
              ))}
            </ListGroup>

            {slide.actions && slide.actions.length
              ? <ListGroup>
                  <ListGroupItem
                    header={slide.actions[0].label}
                    onClick={this._execAction(slide.actions[0])}
                  />
                </ListGroup>
              : ''
            }
          </Col>
        </Row>
      </Grid>
    );
  }
}
