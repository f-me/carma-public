import React from 'react'
import RichTextEditor from 'react-rte'

import {
  Grid, Row, Col,
  ListGroup, ListGroupItem,
  Button, ButtonGroup,
  OverlayTrigger, Tooltip,
  Glyphicon
} from 'react-bootstrap'

import './DiagTree.css';

// h ∈ [0..360]; s ∈ [0..1]; v ∈ [0..1]
// see https://github.com/tmpvar/hsv2rgb/blob/master/hsv2rgb.js
const hsv2rgb = (h, s, v) => {
  h = h % 360;
  s = Math.max(0, Math.min(s, 1));
  v = Math.max(0, Math.min(v, 1));
  if(!s)return [v, v, v].map(x => Math.round(x * 255));
  let res;
  const b = ((1 - s) * v), vb = v - b, hm = h % 60;
  switch ((h / 60) | 0) {
    case 0: res = [v, vb * h / 60 + b, b]; break;
    case 1: res = [vb * (60 - hm) / 60 + b, v, b]; break;
    case 2: res = [b, v, vb * hm / 60 + b]; break;
    case 3: res = [b, vb * (60 - hm) / 60 + b, v]; break;
    case 4: res = [vb * hm / 60 + b, b, v]; break;
    case 5: res = [v, b, vb * (60 - hm) / 60 + b]; break;
  }
  return res.map(x => Math.round(x * 255));
};

const backgrounds = [...Array(7).keys()]
  .map(x => hsv2rgb(x / 7 * 360, 0.10, 1))
  .map(([r, g, b]) => ({backgroundColor: `rgb(${r}, ${g}, ${b})`}));

const yesNoRegs = [/^да/i, /^нет/i];

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


  _loadHistory = () =>
    fetch(`/diag/history/${this.props.caseId}`, {credentials: 'same-origin'})
      .then(resp => resp.json())
      .then(hist => this.setState({
        history: hist,
        slideId: hist[hist.length-1].id
      }))


  _answer = (slideId, ix, nextSlide) => () =>
    fetch(`/_/DiagHistory/${slideId}`,
      { method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        credentials: 'same-origin',
        body: JSON.stringify({answerIx: ix}),
      })
    .then(resp => {
      if (resp.status === 200) {
        fetch('/_/DiagHistory',
          { method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            credentials: 'same-origin',
            body: JSON.stringify({
              caseId: Number.parseInt(this.props.caseId, 10),
              slideId: nextSlide
            })
          })
          .then(this._loadHistory)
      }
    })


  _repeatQuestion = histId => () =>
    fetch(`/diag/retry/${histId}`,
      { method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        credentials: 'same-origin',
        body: "{}",
      })
      .then(this._loadHistory)


  _execAction = act => () =>
    fetch(`/_/${act.svc}`,
      { method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        credentials: 'same-origin',
        body: JSON.stringify({
          parentId: Number.parseInt(this.props.caseId, 10)
        })
      })
      .then(() => {
        window.localStorage.setItem(
            `DiagTree/${this.props.caseId}/newSvc`,
            true)
        alert('Готово. Опрос будет закрыт.')
        window.close()
      })


  _answerItem = slide => (ans, i) => {
    return <ListGroupItem
      header={ans.header}
      className={slide.answerIx === i ? 'selected' : null}
      style={backgrounds[i % backgrounds.length]}
      onClick={slide.answerIx !== null
        ? undefined
        : this._answer(slide.id, i, ans.nextSlide)}
    >
      {ans.file && <img src={ans.file} role="presentation"/>}
      {ans.text}
    </ListGroupItem>;
  };


  render() {
    const {history, slideId, hoverId} = this.state;
    if (!history) return <span>Loading...</span>;

    const slide = history.find(h => h.id === slideId);
    const body  = RichTextEditor.createValueFromString(slide.body, 'markdown');
    const answer = (hist, opt={}) => {
      if(hist.answerIx === null) return null;
      return (<div>
        <div className="history-answer">
          {hist.answers[hist.answerIx].header}
          <br/>
          {hist.answerTime} − {hist.answeredBy}
        </div>
        { !opt.disabled && hoverId ===  hist.id
          ? <OverlayTrigger
                placement="top"
                overlay={<Tooltip>Повторить вопрос</Tooltip>}>
              <Glyphicon
                  className="btn floating-btn"
                  onClick={this._repeatQuestion(hist.id)}
                  glyph="repeat"/>
            </OverlayTrigger>
          : null
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
                      { prevHistory.length && this.state.showDeprecated !== h.id
                        ? <a className="more" href="#"
                            onClick={() => this.setState({showDeprecated: h.id})}
                          >
                            Показать отменённые ответы
                          </a>
                        : null
                      }
                      { this.state.showDeprecated === h.id
                        ? <a className="more" href="#"
                            onClick={() => this.setState({showDeprecated: null})}
                          >
                            Скрыть отменённые ответы
                          </a>
                        : null
                      }
                    </div>
                    { prevHistory.length && this.state.showDeprecated === h.id
                      ? <ListGroup>
                        { history
                            .filter(hh => hh.deprecatedBy === h.id)
                            .map((hh, j) =>
                              <ListGroupItem key={j}
                                className={hh.id === slideId
                                            ? 'deprecated selected'
                                            : 'deprecated'}
                                header={hh.header}
                                onClick={() => this.setState({slideId: hh.id})}
                              >
                                {answer(h, {disabled: true})}
                              </ListGroupItem>
                            )
                        }
                        </ListGroup>
                      : null
                    }
                    <ListGroupItem
                      className={h.id === slideId ? 'selected' : null}
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

            { slide.answers.length === 2 &&
              ( yesNoRegs.every((x, i) => x.test(slide.answers[i].header)) ||
                yesNoRegs.reverse().every((x, i) => x.test(slide.answers[i].header))
              )

              ? <Row>
                  {slide.answers.map((x, i) =>
                    <Col md={6}>
                      <ListGroup>{this._answerItem(slide)(x, i)}</ListGroup>
                    </Col>
                  )}
                </Row>

              : <ListGroup>
                  {slide.answers.map(this._answerItem(slide))}
                </ListGroup>
            }

            {slide.actions && slide.actions.length
              ? <ListGroup>
                  <ListGroupItem
                    header={slide.actions[0].label}
                    onClick={this._execAction(slide.actions[0])}
                  />
                </ListGroup>
              : null
            }
          </Col>
        </Row>
      </Grid>
    );
  }
}
