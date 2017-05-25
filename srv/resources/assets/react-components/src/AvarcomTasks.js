import React, { Component } from 'react';

import Typeahead from 'react-bootstrap-typeahead';
import 'react-bootstrap-typeahead/css/Typeahead.css';
import './AvarcomTasks.css';

const propTypes = {
};

const defaultProps = {
};

let avarcomTasks = null;

const DISABLED_STATES = [4, 9, 19, 20];


export default class AvarcomTasks extends Component {
  constructor(props) {
    super(props);

    this.state = {
      availableTasks: avarcomTasks || [],
      selectedTasks: this.props.value,
      disabled: DISABLED_STATES.indexOf(this.props.kvm.status()) >= 0
    };

    this.props.kvm.status.subscribe(st =>
      this.setState({disabled: DISABLED_STATES.indexOf(st) >= 0})
    );

    if (avarcomTasks) {
      this._setSelectedTasks(this.props.value);
    } else {
      fetch('/_/AvarcomTask', {credentials: 'same-origin'})
        .then(resp => resp.json().then(jsn => {
          avarcomTasks = jsn;
          this._setSelectedTasks(this.state.selectedTasks);
        }))
    }
  }

  _setSelectedTasks = (selectedTasks, callback) => {
    this.setState(
      { selectedTasks,
        availableTasks: avarcomTasks
          .filter(x => x.isActive && !selectedTasks.find(y => x.id === y.id))
          .map(x => ({isChecked: false, id: x.id, label: x.label}))
      },
      callback
    );
  }

  _addTask = tasks => {
    if (tasks.length === 0) return;
    this._setSelectedTasks(
      this.state.selectedTasks.concat(
        tasks.map(x => ({isChecked: false, id: x.id, label: x.label}))),
      () => {
        this.refs.typeahead.getInstance().clear();
        this.props.onChange(this.state.selectedTasks);
      }
    );
  }

  _checkTask = task => {
    return ev => {
      task.isChecked = ev.target.checked;
      this.forceUpdate(() =>
        this.props.onChange(this.state.selectedTasks));
    }
  }

  _removeTask = task => {
    return ev => {
      ev.preventDefault();
      this._setSelectedTasks(
        this.state.selectedTasks.filter(t => t.id !== task.id),
        () => this.props.onChange(this.state.selectedTasks)
      );
    }
  }

  render() {
    if (this.state.disabled) {
      const selectedTasks = this.state.selectedTasks.map(task => {
        return (
          <div key={task.id}>
            <input type="checkbox" disabled checked={task.isChecked}/>
            &nbsp; {task.label}
            <br/>
          </div>);
      });

      return (<div>{selectedTasks}</div>);
    }
    else {
      const selectedTasks = this.state.selectedTasks.map(task => {
        return (
          <div key={task.id}>
            <input type="checkbox"
              checked={task.isChecked}
              onChange={this._checkTask(task)}
            />
            &nbsp; {task.label} &nbsp;
            <a href="" onClick={this._removeTask(task)}>×</a>
            <br/>
          </div>);
      });

      return (
        <div className="AvarcomTasks">
          <Typeahead emptyLabel="Ничего не найдено"
            ref="typeahead"
            options={this.state.availableTasks}
            selected={[]}
            onChange={this._addTask}
          />
          <div>{selectedTasks}</div>
        </div>
      );
    }
  }
}

AvarcomTasks.propTypes = propTypes;
AvarcomTasks.defaultProps = defaultProps;
